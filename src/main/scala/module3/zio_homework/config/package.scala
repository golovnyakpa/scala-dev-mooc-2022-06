//package module3.zio_homework
//
//import zio.config.ReadError
//import zio.config.typesafe.TypesafeConfig
//import zio.{Layer, Task}
//
//
//package object config {
//   case class AppConfig(appName: String, appUrl: String)
//
//  import zio.config.magnolia.DeriveConfigDescriptor.descriptor
//
//  val configDescriptor = descriptor[AppConfig]
//
//  type Configuration = zio.Has[AppConfig]
//
//  object Configuration{
//    val live: Layer[ReadError[String], Configuration] = TypesafeConfig.fromDefaultLoader(configDescriptor)
//  }
//}
