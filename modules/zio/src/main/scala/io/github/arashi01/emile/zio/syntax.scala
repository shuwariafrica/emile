/*
 * Copyright 2025 the original author(s).
 * SPDX-License-Identifier: MIT
 */
package io.github.arashi01.emile.zio

/**
 * Convenience syntax for emile-zio module.
 *
 * Import this object to get all emile-zio extensions and utilities:
 * {{{
 * import io.github.arashi01.emile.zio.syntax.all.*
 * }}}
 *
 * Or import selectively:
 * {{{
 * import io.github.arashi01.emile.zio.syntax.errors.*
 * import io.github.arashi01.emile.zio.syntax.config.given
 * }}}
 */
object syntax:
  /**
   * All emile-zio syntax.
   *
   * Exports:
   * - Error extensions (.toZIO, .catchEmile, .recoverEmile, .mapEmileError)
   * - Emile pattern extractor for error handlers
   * - Config given instances for ZIO.config[T]
   * - EmileLoop service accessors
   */
  object all:
    // Error syntax
    export io.github.arashi01.emile.zio.{toZIO, catchEmile, recoverEmile, mapEmileError, Emile}

    // Config givens
    export io.github.arashi01.emile.zio.{loopConfig, tcpConfig, emileConfig, tcpKeepAliveConfig}

    // EmileLoop accessors
    export io.github.arashi01.emile.zio.EmileLoop.{loop, runOnce, runNoWait, runUntilComplete, stop}

  /**
   * Error handling syntax only.
   */
  object errors:
    export io.github.arashi01.emile.zio.{toZIO, catchEmile, recoverEmile, mapEmileError, Emile}

  /**
   * Config descriptors as givens.
   */
  object config:
    export io.github.arashi01.emile.zio.{loopConfig, tcpConfig, emileConfig, tcpKeepAliveConfig}
end syntax
