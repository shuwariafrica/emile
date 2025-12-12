/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.unsafe

import scala.annotation.internal.sharable
import scala.collection.mutable.LongMap
import scala.scalanative.unsafe.*

/**
 * Registry for mapping stable Long IDs to Scala callback objects.
 *
 * This registry pattern provides GC-safe callback management for libuv handles.
 * Instead of storing raw object pointers in libuv's `data` field (which could become
 * invalid if the GC moves objects), we store a stable Long ID that maps to the
 * actual callback object in this registry.
 *
 * == Design Rationale ==
 *
 * - Scala Native's current Immix/Commix GC is mark-sweep (non-moving), but future
 *   GC implementations may use moving collectors
 * - Storing raw pointers to GC-managed objects in C structures is inherently unsafe
 * - The registry pattern uses stable Long IDs that remain valid regardless of GC behavior
 * - The registry keeps callbacks rooted (preventing premature collection) while registered
 * - ID 0 is reserved as a sentinel value meaning "no callback registered"
 *
 * == Thread Safety Warning ==
 *
 * '''This implementation is NOT thread-safe.'''
 *
 * While libuv event loops are single-threaded, libuv does support running multiple
 * independent event loops on different threads simultaneously. This global registry
 * would cause data races if used with multiple loops across threads.
 *
 * '''Current limitation:''' Only use one event loop at a time, or ensure all loop
 * operations (including handle creation/destruction) happen on the same thread.
 *
 * '''Future work:''' To support multi-loop scenarios, consider:
 * - Scope a registry per loop via `uv_loop_set_data`/`uv_loop_get_data`
 * - Add synchronization (e.g., `scala.scalanative.runtime.Monitors`)
 * - Use thread-local storage for per-thread registries
 *
 * @see [[https://docs.libuv.org/en/stable/guide/basics.html#event-loops libuv Event Loops]]
 * @see [[https://scala-native.org/en/stable/user/interop.html Scala Native Interop Guide]]
 *
 * @note The `@sharable` annotation tells `-Ycheck-reentrant` that this global mutable
 *       state is intentionally shared. This suppresses the compiler warning but does
 *       NOT provide thread safety.
 */
@sharable
private[emile] object CallbackRegistry:
  /** Counter for generating unique callback IDs. Starts at 1 so 0 can be the sentinel. */
  private var nextId: Long = 1L

  /** Map from stable IDs to callback objects. */
  private val callbacks: LongMap[Any] = LongMap.empty

  /**
   * Register a callback and return its stable ID.
   *
   * The returned ID can be safely stored in libuv's handle `data` field
   * and later used to retrieve the callback.
   *
   * @param callback The callback object to register
   * @return A stable Long ID for this callback
   */
  def register(callback: Any): Long =
    val id = nextId
    nextId += 1
    val _ = callbacks.put(id, callback)
    id

  /**
   * Retrieve a callback by its ID.
   *
   * @param id The callback ID returned from `register`
   * @return The callback object, or throws if not found
   */
  def get(id: Long): Any =
    callbacks.getOrElse(id, throw new NoSuchElementException(s"Callback not found: $id"))

  /**
   * Retrieve a callback by its ID with type casting.
   *
   * @tparam A The expected callback type
   * @param id The callback ID returned from `register`
   * @return The callback object cast to type A
   */
  def getAs[A](id: Long): A =
    get(id).asInstanceOf[A]

  /**
   * Retrieve a callback by its ID, returning None if not found.
   *
   * @param id The callback ID
   * @return Some(callback) if found, None otherwise
   */
  def find(id: Long): Option[Any] =
    callbacks.get(id)

  /**
   * Retrieve a callback by its ID with type casting, returning None if not found.
   *
   * @tparam A The expected callback type
   * @param id The callback ID
   * @return Some(callback) cast to type A if found, None otherwise
   */
  def findAs[A](id: Long): Option[A] =
    callbacks.get(id).map(_.asInstanceOf[A])

  /**
   * Unregister a callback, allowing it to be garbage collected.
   *
   * This should be called when a handle is closed to prevent memory leaks.
   *
   * @param id The callback ID to unregister
   * @return true if the callback was found and removed, false otherwise
   */
  def unregister(id: Long): Boolean =
    callbacks.remove(id).isDefined

  /**
   * Check if a callback ID is registered.
   *
   * @param id The callback ID to check
   * @return true if registered, false otherwise
   */
  def contains(id: Long): Boolean =
    callbacks.contains(id)

  /**
   * Get the number of registered callbacks.
   *
   * Useful for debugging and detecting callback leaks.
   *
   * @return The number of currently registered callbacks
   */
  def size: Int =
    callbacks.size

  /**
   * Clear all registered callbacks.
   *
   * This is primarily for testing. In normal use, callbacks should be
   * unregistered individually when handles are closed.
   */
  def clear(): Unit =
    callbacks.clear()
    nextId = 1L  // Reset to 1, not 0, to preserve sentinel invariant

end CallbackRegistry

/**
 * Utilities for storing callback IDs in libuv handle data pointers.
 *
 * These utilities convert between Long callback IDs and the raw pointers
 * that libuv expects in its `data` fields.
 *
 * We store the Long ID directly as a pointer value (reinterpreting the bits).
 * This is safe because:
 * 1. We never dereference these "pointers" - they're just opaque storage
 * 2. libuv's data field is just void* storage, not actual memory access
 * 3. On 64-bit systems, pointers and Long are both 64 bits
 */
private[emile] object CallbackIdUtils:
  import scala.scalanative.runtime.Intrinsics.{castLongToRawPtr, castRawPtrToLong}
  import scala.scalanative.runtime.{fromRawPtr, toRawPtr}

  /**
   * Convert a Long callback ID to a Ptr[Byte] for storage in libuv data field.
   *
   * This reinterprets the Long bits as a pointer value (zero allocation).
   */
  inline def idToPtr(id: Long): Ptr[Byte] =
    fromRawPtr[Byte](castLongToRawPtr(id))

  /**
   * Convert a Ptr[Byte] from libuv data field back to a Long callback ID.
   *
   * This reinterprets the pointer bits as a Long value (zero allocation).
   */
  inline def ptrToId(ptr: Ptr[Byte]): Long =
    castRawPtrToLong(toRawPtr(ptr))

  /**
   * Set a callback ID on a libuv handle's data field.
   *
   * Uses libuv's uv_handle_set_data API for correct access.
   *
   * @param handle The handle pointer
   * @param id The callback ID to store
   */
  inline def setCallbackId(handle: Ptr[Byte], id: Long): Unit =
    LibUV.uv_handle_set_data(handle, idToPtr(id))

  /**
   * Get the callback ID from a libuv handle's data field.
   *
   * Uses libuv's uv_handle_get_data API for correct access.
   *
   * @param handle The handle pointer
   * @return The stored callback ID
   */
  inline def getCallbackId(handle: Ptr[Byte]): Long =
    ptrToId(LibUV.uv_handle_get_data(handle))

  /**
   * Clear the callback ID from a libuv handle's data field.
   *
   * @param handle The handle pointer
   */
  inline def clearCallbackId(handle: Ptr[Byte]): Unit =
    LibUV.uv_handle_set_data(handle, idToPtr(0L))

end CallbackIdUtils
