/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.unsafe

import munit.FunSuite

/**
 * Tests for CallbackRegistry.
 *
 * These tests verify the registry pattern for GC-safe callback storage.
 */
class CallbackRegistrySuite extends FunSuite:

  override def beforeEach(context: BeforeEach): Unit =
    // Clear registry before each test
    CallbackRegistry.clear()

  test("register returns unique IDs"):
    val id1 = CallbackRegistry.register("callback1")
    val id2 = CallbackRegistry.register("callback2")
    val id3 = CallbackRegistry.register("callback3")

    assertNotEquals(id1, id2)
    assertNotEquals(id2, id3)
    assertNotEquals(id1, id3)

  test("get retrieves registered callback"):
    val callback = () => println("test")
    val id = CallbackRegistry.register(callback)

    val retrieved = CallbackRegistry.get(id)
    assertEquals(retrieved, callback)

  test("getAs retrieves callback with correct type"):
    val callback: () => Int = () => 42
    val id = CallbackRegistry.register(callback)

    val retrieved = CallbackRegistry.getAs[() => Int](id)
    assertEquals(retrieved(), 42)

  test("find returns Some for registered callback"):
    val callback = "test-callback"
    val id = CallbackRegistry.register(callback)

    val found = CallbackRegistry.find(id)
    assert(found.isDefined)
    assertEquals(found.get, callback)

  test("find returns None for unregistered ID"):
    val found = CallbackRegistry.find(99999L)
    assert(found.isEmpty)

  test("findAs returns correctly typed Option"):
    val callback: Int => String = n => s"number: $n"
    val id = CallbackRegistry.register(callback)

    val found = CallbackRegistry.findAs[Int => String](id)
    assert(found.isDefined)
    assertEquals(found.get(42), "number: 42")

  test("unregister removes callback"):
    val id = CallbackRegistry.register("to-remove")
    assert(CallbackRegistry.contains(id))

    val removed = CallbackRegistry.unregister(id)
    assert(removed)
    assert(!CallbackRegistry.contains(id))

  test("unregister returns false for unknown ID"):
    val removed = CallbackRegistry.unregister(99999L)
    assert(!removed)

  test("contains returns correct state"):
    val id = CallbackRegistry.register("test")

    assert(CallbackRegistry.contains(id))
    assert(!CallbackRegistry.contains(99999L))

    val _ = CallbackRegistry.unregister(id)
    assert(!CallbackRegistry.contains(id))

  test("size tracks registered callbacks"):
    assertEquals(CallbackRegistry.size, 0)

    val id1 = CallbackRegistry.register("a")
    assertEquals(CallbackRegistry.size, 1)

    val id2 = CallbackRegistry.register("b")
    assertEquals(CallbackRegistry.size, 2)

    val _ = CallbackRegistry.unregister(id1)
    assertEquals(CallbackRegistry.size, 1)

    val _ = CallbackRegistry.unregister(id2)
    assertEquals(CallbackRegistry.size, 0)

  test("clear removes all callbacks"):
    val _ = CallbackRegistry.register("a")
    val _ = CallbackRegistry.register("b")
    val _ = CallbackRegistry.register("c")
    assertEquals(CallbackRegistry.size, 3)

    CallbackRegistry.clear()
    assertEquals(CallbackRegistry.size, 0)

  test("IDs restart from 1 after clear"):
    val id1 = CallbackRegistry.register("first")
    CallbackRegistry.clear()
    val id2 = CallbackRegistry.register("second")

    // After clear, IDs should restart from 1 (0 is sentinel for "no callback")
    assertEquals(id2, 1L)

  test("first registered ID is 1 not 0"):
    // ID 0 is reserved as sentinel meaning "no callback"
    val firstId = CallbackRegistry.register("first")
    assertEquals(firstId, 1L, "First callback ID should be 1, not 0 (0 is sentinel)")

  test("ID 0 is never assigned"):
    // Register many callbacks and verify none get ID 0
    val ids = (0 until 100).map(i => CallbackRegistry.register(s"cb-$i"))
    assert(!ids.contains(0L), "ID 0 should never be assigned (reserved as sentinel)")

  test("registry handles many callbacks"):
    val ids = (0 until 1000).map { i =>
      CallbackRegistry.register(s"callback-$i")
    }

    assertEquals(CallbackRegistry.size, 1000)
    assertEquals(ids.toSet.size, 1000) // All IDs unique

    // Verify all can be retrieved
    ids.zipWithIndex.foreach { case (id, i) =>
      assertEquals(CallbackRegistry.get(id), s"callback-$i")
    }

end CallbackRegistrySuite
