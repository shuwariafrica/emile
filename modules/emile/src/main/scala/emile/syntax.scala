/*
 * Copyright 2025, 2026 Ali Rashid
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package emile

import boilerplate.effect.EffIO
import cats.effect.kernel.Resource
import fs2.Pipe
import fs2.Stream

/** Typed-error effect for the libuv runtime - a `cats.effect.IO` carrying an `Either[E, A]`,
  * covariant in both the error `E` and the value `A`, so a narrower error widens implicitly at
  * every call site.
  */
type EmIO[+E, +A] = EffIO[E, A]

/** Companion namespace for the [[EmIO]] effect alias; holds its higher-kinded partial application
  * [[EmIO.Of]].
  */
object EmIO:

  /** The effect constructor with its error fixed to `E`, `[A] =>> EmIO[E, A]`, for the
    * type-constructor positions of `Stream`, `Resource`, and `Pipe`. Mirrors `EffIO.Of`.
    */
  type Of[E] = EffIO.Of[E]

/** An fs2 `Stream` scoped over the [[EmIO]] effect; covariant in the error `E`. */
type EmStream[+E, +A] = Stream[EffIO.Of[E], A]

/** A cats-effect `Resource` scoped over the [[EmIO]] effect. Invariant in `E` - `Resource` is
  * invariant in its effect type - so the error channel is widened through the `widen` extension
  * rather than implicitly.
  */
type EmResource[E, A] = Resource[EffIO.Of[E], A]

/** An fs2 `Pipe` scoped over the [[EmIO]] effect. Invariant in `E` and so not error-widenable; it
  * is applied with `through` to a stream of its own error type.
  */
type EmPipe[E, -I, +O] = Pipe[EffIO.Of[E], I, O]

/** Widens an [[EmResource]]'s error channel to a supertype - the explicit counterpart to the
  * implicit widening that the covariant [[EmIO]] and [[EmStream]] get for free.
  */
extension [E, A](resource: EmResource[E, A]) def widen[E2 >: E]: EmResource[E2, A] = resource.mapK(EffIO.widenK[E, E2])

/** Widens an [[EmStream]]'s error channel to a supertype without a cast. [[EmStream]] also widens
  * implicitly through covariance; this is the explicit, inference-friendly spelling, paralleling
  * the `widen` on [[EmResource]].
  */
extension [E, A](stream: EmStream[E, A]) def widenS[E2 >: E]: EmStream[E2, A] = stream.translate(EffIO.widenK[E, E2])

/** Widens an [[EmPipe]]'s error channel to a supertype. `Pipe` is invariant in its effect (the
  * effect occupies a function-input position), so it cannot widen structurally as [[EmIO]] and
  * [[EmStream]] do; widening the resulting stream with `widenS` is the cast-free alternative.
  */
extension [E, I, O](pipe: EmPipe[E, I, O])
  def widen[E2 >: E]: EmPipe[E2, I, O] =
    // EmIO's error is a phantom (erased at runtime), so reinterpreting the invariant Pipe at a wider
    // error is a runtime identity, not a structural transform, exactly as EffIO.assumeError does.
    pipe.asInstanceOf[EmPipe[E2, I, O]] // scalafix:ok DisableSyntax.asInstanceOf
