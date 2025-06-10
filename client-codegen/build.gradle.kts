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
            groupId = "io.superposition.smithy.haskell"
            artifactId = "client-codegen"
            version = "0.0.1-rc1"

            from(components["kotlin"])
        }
    }
    repositories {
        maven {
            url = uri(layout.buildDirectory.dir("m2"))
        }
    }
}
