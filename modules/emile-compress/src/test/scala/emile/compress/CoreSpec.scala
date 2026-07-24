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
package emile.compress

// Covers the shared vocabulary: the Budget ceiling arithmetic and the idempotent Unexpected.
final class CoreSpec extends munit.FunSuite:

  test("Budget enforces its ceiling and unlimited never exceeds") {
    val budget = Budget.bytes(100)
    assert(!budget.exceededBy(100), "a total at the limit is within budget")
    assert(budget.exceededBy(101), "a total above the limit exceeds it")
    assert(!budget.isUnlimited)
    assert(Budget.unlimited.isUnlimited)
    assert(!Budget.unlimited.exceededBy(Long.MaxValue), "unlimited never exceeds")
  }

  test("Budget.bytes rejects a negative limit") {
    intercept[IllegalArgumentException](Budget.bytes(-1))
  }

  test("Unexpected returns an existing CompressError unchanged and wraps a raw cause") {
    val truncated: CompressError = CompressError.Truncated
    assertEquals(CompressError.Unexpected(truncated), truncated)
    CompressError.Unexpected(new RuntimeException("boom")) match
      case CompressError.Unexpected(cause) => assertEquals(cause.getMessage, "boom")
      case other => fail(s"expected Unexpected, got $other")
  }
end CoreSpec
