/*
 * Copyright 2025, 2026 Ali Rashid.
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
package emile.ipa

// scalafix:off DisableSyntax.throw; compile-time macro validation throws for invalid literals

import scala.quoted.*

/** String interpolation macros for compile-time validated IP addresses and ports.
  *
  * ==Usage==
  *
  * {{{
  * import emile.ipa.literals.{*, given}
  *
  * val p: Port = port"8080"
  * val v4: Ipv4Address = ipv4"192.168.1.1"
  * val v6: Ipv6Address = ipv6"::1"
  * }}}
  *
  * These interpolators validate the input at compile time and emit a compile-time error for invalid
  * values.
  *
  * ==Interpolation==
  *
  * Literal-only strings are fully validated at compile time. Mixed literal and interpolated values
  * are allowed; literal fragments are validated at compile time while the final value is validated
  * at runtime.
  */
object literals:

  extension (inline ctx: StringContext)
    /** Compile-time validated port literal.
      *
      * {{{
      * val http = port"80"
      * val https = port"443"
      * }}}
      */
    inline def port(inline args: Any*): Port =
      ${ Macros.portImpl('ctx, 'args) }

    /** Compile-time validated IPv4 address literal.
      *
      * {{{
      * val localhost = ipv4"127.0.0.1"
      * val home = ipv4"192.168.1.1"
      * }}}
      */
    inline def ipv4(inline args: Any*): Ipv4Address =
      ${ Macros.ipv4Impl('ctx, 'args) }

    /** Compile-time validated IPv6 address literal.
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

  def portImpl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(
    using Quotes
  ): Expr[Port] =
    import quotes.reflect.report
    val parts = literalParts(ctx)

    if isPureLiteral(args) then
      val str = extractLiteralString(parts)
      str.toIntOption match
        case Some(n) if n >= Port.MinValue && n <= Port.MaxValue =>
          '{ Port.wrap(${ Expr(n) }) }
        case Some(_) =>
          report.errorAndAbort(s"Port must be in range 0-65535: $str")
        case None =>
          report.errorAndAbort(s"Invalid port number: $str")
    else
      validatePortFragments(parts)
      '{ MacrosRuntime.parsePort($ctx, $args) }
    end if
  end portImpl

  def ipv4Impl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(
    using Quotes
  ): Expr[Ipv4Address] =
    import quotes.reflect.report
    val parts = literalParts(ctx)

    if isPureLiteral(args) then
      val str = extractLiteralString(parts)
      Ipv4Address.from(str) match
        case Right(addr) =>
          val int = addr.toInt
          '{ Ipv4Address.fromInt(${ Expr(int) }) }
        case Left(err) =>
          report.errorAndAbort(err.message)
    else
      validateIpv4Fragments(parts)
      '{ MacrosRuntime.parseIpv4($ctx, $args) }
  end ipv4Impl

  def ipv6Impl(ctx: Expr[StringContext], args: Expr[Seq[Any]])(
    using Quotes
  ): Expr[Ipv6Address] =
    import quotes.reflect.report
    val parts = literalParts(ctx)

    if isPureLiteral(args) then
      val str = extractLiteralString(parts)
      Ipv6Address.from(str) match
        case Right(addr) =>
          val high = addr.highBits
          val low = addr.lowBits
          '{ Ipv6Address.fromLongs(${ Expr(high) }, ${ Expr(low) }) }
        case Left(err) =>
          report.errorAndAbort(err.message)
    else
      validateIpv6Fragments(parts)
      '{ MacrosRuntime.parseIpv6($ctx, $args) }
    end if
  end ipv6Impl

  private def isPureLiteral(args: Expr[Seq[Any]])(using Quotes): Boolean =
    args match
      case Varargs(Nil) => true
      case '{ Seq() }   => true
      case _            => false

  private def literalParts(ctx: Expr[StringContext])(using Quotes): List[String] =
    import quotes.reflect.report
    ctx match
      case '{ StringContext(${ Varargs(parts) }*) } =>
        parts.toList.map {
          case Expr(s: String) => s
          case _               => report.errorAndAbort("Expected literal string parts")
        }
      case _ =>
        report.errorAndAbort("Expected a literal string context")

  private def extractLiteralString(parts: List[String])(using Quotes): String =
    import quotes.reflect.report
    parts match
      case head :: Nil => head
      case _           => report.errorAndAbort("Interpolation not supported - use a literal string")

  private def validatePortFragments(parts: List[String])(using Quotes): Unit =
    import quotes.reflect.report
    parts.find(p => p.nonEmpty && !p.forall(_.isDigit)).foreach { bad =>
      report.errorAndAbort(s"Invalid port literal fragment: '$bad'")
    }

  private def validateIpv4Fragments(parts: List[String])(using Quotes): Unit =
    import quotes.reflect.report
    val validChar: Char => Boolean = c => c.isDigit || c == '.'
    parts.find(p => p.exists(ch => !validChar(ch))).foreach { bad =>
      report.errorAndAbort(s"Invalid IPv4 literal fragment: '$bad'")
    }

  private def validateIpv6Fragments(parts: List[String])(using Quotes): Unit =
    import quotes.reflect.report
    val validChar: Char => Boolean = c => c.isDigit || c == ':' || c == '.' || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    parts.find(p => p.exists(ch => !validChar(ch))).foreach { bad =>
      report.errorAndAbort(s"Invalid IPv6 literal fragment: '$bad'")
    }

end Macros

private[ipa] object MacrosRuntime:
  def parsePort(ctx: StringContext, args: Seq[Any]): Port =
    val value = ctx.s(args*)
    Port.from(value) match
      case Right(p)  => p
      case Left(err) => throw IllegalArgumentException(err.message)

  def parseIpv4(ctx: StringContext, args: Seq[Any]): Ipv4Address =
    val value = ctx.s(args*)
    Ipv4Address.from(value) match
      case Right(addr) => addr
      case Left(err)   => throw IllegalArgumentException(err.message)

  def parseIpv6(ctx: StringContext, args: Seq[Any]): Ipv6Address =
    val value = ctx.s(args*)
    Ipv6Address.from(value) match
      case Right(addr) => addr
      case Left(err)   => throw IllegalArgumentException(err.message)
end MacrosRuntime
