package com.rockthejvm.jobsboard.config

import pureconfig.generic.derivation.default.*
import pureconfig.ConfigReader
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import pureconfig.error.CannotConvert

//automatically generate an implicit/given configReader: ConfigReader[EmberConfig]
final case class EmberConfig(host: Host, port: Port) derives ConfigReader

/*
PureConfig can't automatically derive the ConfigReader for the EmberConfig case class.

The problem is related to Host and Port being non-primitive types, but there is more to it:
Automatic Derivation fails because:

  PureConfig's automatic derivation works by:
  1. Looking at each field in your case class
  2. Trying to find a ConfigReader instance for each field's
  type
  3. Combining these readers to create a reader for the whole
  case class

The derivation succeeds when PureConfig can find ConfigReader
  instances for all field types. It comes with built-in
readers for:
- Primitive types (String, Int, Boolean, etc.)
- Common Scala types (Option, List, Map, etc.)
- Java time types (LocalDate, Duration, etc.)

However, Host and Port are from the com.comcast.ip4s library
(used by http4s), and PureConfig doesn't have built-in
ConfigReader instances for these types.

We must therefore provide explicit custom ConfigReader instances for Host and Port because:

1. Domain-specific types: Host and Port are domain-specific
types with their own validation rules (e.g., valid port
ranges 0-65535, valid hostname formats)
2. No universal conversion: There's no single "correct" way
to convert a string to these types - should "localhost"
become an IPv4 host, IPv6 host, or DNS name?
3. Error handling: These types can fail construction (e.g.,
port -1 is invalid), and PureConfig needs to know how to
handle these failures


The beauty of this approach is that once these readers are in
scope, the derives ConfigReader on your case class will work
automatically, combining these custom readers with
PureConfig's derivation machinery.

 */

object EmberConfig {

  given hostReader: ConfigReader[Host] = ConfigReader[String].emap { hostString =>
    Host.fromString(hostString) match {
      case Some(host) => Right(host)
      case None =>
        Left(CannotConvert(hostString, Host.getClass.toString, s"Invalid host: $hostString"))
    }
  }

  given portReader: ConfigReader[Port] = ConfigReader[Int].emap { portInt =>
    // a more concise version of the pattern match above:
    Port
      .fromInt(portInt)
      .toRight(
        // case None...
        CannotConvert(portInt.toString, Port.getClass.toString, s"Invalid port: $portInt")
      )
  }

}
