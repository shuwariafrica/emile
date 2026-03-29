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

import java.net.NetworkInterface

given platformScopeId: ScopeIdPlatform with
  def fromInterfaceName(name: String): Either[String, ScopeId] =
    val nic = NetworkInterface.getByName(name)
    if nic == null then Left(s"unknown network interface '$name'") // scalafix:ok DisableSyntax.null
    else
      val idx = nic.getIndex
      if idx <= 0 then Left(s"network interface '$name' has invalid index $idx")
      else Right(ScopeId(idx))
