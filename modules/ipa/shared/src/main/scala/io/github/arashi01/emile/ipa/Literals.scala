/*
 * Copyright 2025 the original author(s).
 *
 * Licensed under the MIT License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/MIT
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.arashi01.emile.ipa

import scala.quoted.*

/**
 * String interpolation macros for compile-time validated IP addresses and
 * ports.
 *
 * == Usage ==
 *
 * {{{
 * import io.github.arashi01.emile.ipa.literals.{*, given}
 *
 * val p: Port = port"8080"
 * val v4: Ipv4Address = ipv4"192.168.1.1"
 * val v6: Ipv6Address = ipv6"::1"
 * }}}
 *
 * These interpolators validate the input at compile time and emit a
 * compile-time error for invalid values.
 *
 * == Interpolation ==
 *
 * Currently only literal strings are supported (no interpolation variables).
 * This allows full compile-time validation.
 */
object literals:

  extension (inline ctx: StringContext)
    /**
     * Compile-time validated port literal.
     *
     * {{{
     * val http = port"80"
     * val https = port"443"
     * }}}
     */
    inline def port(inline args: Any*): Port =
      ${ Macros.portImpl('ctx, 'args) }

    /**
     * Compile-time validated IPv4 address literal.
     *
     * {{{
     * val localhost = ipv4"127.0.0.1"
     * val home = ipv4"192.168.1.1"
     * }}}
     */
    inline def ipv4(inline args: Any*): Ipv4Address =
      ${ Macros.ipv4Impl('ctx, 'args) }

    /**
     * Compile-time validated IPv6 address literal.
     *
     * {{{
     * val loopback = ipv6"::1"
     * val example = ipv6"2001:db8::1"
     * }}}
     */
    inline def ipv6(inline args: Any*): Ipv6Address =
      ${ Macros.ipv6Impl('ctx, 'args) }

  end extension

end literals

private object Macros:

  def portImpl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(using
      Quotes
  ): Expr[Port] =
    import quotes.reflect.*

    val str = extractLiteralString(ctx, args)

    str.toIntOption match
      case Some(n) if n >= Port.MinValue && n <= Port.MaxValue =>
        '{ Port.unsafeFromInt(${ Expr(n) }) }
      case Some(_) =>
        report.errorAndAbort(s"Port must be in range 0-65535: $str")
      case None =>
        report.errorAndAbort(s"Invalid port number: $str")

  def ipv4Impl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(using
      Quotes
  ): Expr[Ipv4Address] =
    import quotes.reflect.*

    val str = extractLiteralString(ctx, args)

    Ipv4Address.parse(str) match
      case Right(addr) =>
        val int = addr.toInt
        '{ Ipv4Address.fromInt(${ Expr(int) }) }
      case Left(err) =>
        report.errorAndAbort(err.message)

  def ipv6Impl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(using
      Quotes
  ): Expr[Ipv6Address] =
    import quotes.reflect.*

    val str = extractLiteralString(ctx, args)

    Ipv6Address.parse(str) match
      case Right(addr) =>
        val high = addr.highBits
        val low  = addr.lowBits
        '{ Ipv6Address.fromLongs(${ Expr(high) }, ${ Expr(low) }) }
      case Left(err) =>
        report.errorAndAbort(err.message)

  private def extractLiteralString(
      ctx: Expr[StringContext],
      args: Expr[Seq[Any]]
  )(using Quotes): String =
    import quotes.reflect.*

    // Verify no interpolation arguments
    args match
      case '{ Seq() }          => () // OK - no args
      case '{ Seq(${ _ }*) }   => report.errorAndAbort("Interpolation not supported - use a literal string")
      case _ => () // Assume empty for now

    // Extract the string parts from the StringContext
    ctx match
      case '{ StringContext(${ Varargs(parts) }*) } =>
        parts.toList match
          case List(Expr(s: String)) => s
          case List(part) =>
            report.errorAndAbort("Expected a literal string")
          case _ =>
            report.errorAndAbort("Interpolation not supported - use a literal string")
      case _ =>
        report.errorAndAbort("Expected a literal string context")

end Macros
