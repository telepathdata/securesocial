name := "ssroot"

version := "1.0"

lazy val root = project.in(file(".")).aggregate(module, ssdemo)

lazy val module = project.in(file("./module-code"))

lazy val ssdemo = project.in(file("./samples/scala/demo"))
  .dependsOn(module)
