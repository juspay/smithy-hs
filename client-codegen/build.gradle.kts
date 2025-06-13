plugins {
    alias(libs.plugins.kotlin.jvm)
    `maven-publish`
}

repositories {
    mavenCentral()
}

dependencies {
    api(libs.smithy.codegen.core)
    api(libs.smithy.model)
    api(libs.smithy.protocol.traits)
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            groupId = "in.juspay.smithy.haskell"
            artifactId = "client-codegen"
            version = "0.0.1-dev"

            from(components["kotlin"])
        }
    }
    repositories {
        maven {
            url = uri(layout.buildDirectory.dir("m2"))
        }
    }
}
