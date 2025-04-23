
name := "TypeLevelNumbers"

scalaVersion := "3.7.0-RC3"

scalacOptions ++= Seq(
//  "-Vprofile",
//  "-Werror",
  "-Wimplausible-patterns",
  "-Wnonunit-statement",
  "-Wsafe-init",
  "-Wunused:all",
  "-Xcook-docs",
  "-Xdebug-macros",
//  "-Xdisable-assertions",
  "-Xkind-projector:underscores",
//  "-Xmax-inlines", "128",
  "-Ycheck-all-patmat",
//  "-Ycheck-reentrant",
  "-Ydebug-pos",
  "-Yexplicit-nulls",
  "-Yrequire-targetName",
  "-Ysafe-init-global",
  "-deprecation",
  "-experimental",
  "-explain",
  "-feature",
//  "-language:experimental.captureChecking",
//  "-language:experimental.genericNumberLiterals",
//  "-language:experimental.into",
//  "-language:experimental.modularity",
//  "-language:experimental.namedTypeArguments",
//  "-language:experimental.pureFunctions",
  "-language:strictEquality",
  "-new-syntax",
  "-release:21",
  "-source:future",
  "-unchecked",
)
