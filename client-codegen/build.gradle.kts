plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.dokka)
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

java {
    withSourcesJar()
}

tasks.register<Jar>("dokkaJavadocJar") {
    dependsOn(tasks.dokkaJavadoc)
    from(tasks.dokkaJavadoc.flatMap { it.outputDirectory })
    archiveClassifier.set("javadoc")
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            from(components["kotlin"])
            artifact(tasks["sourcesJar"])
            artifact(tasks["dokkaJavadocJar"])
            pom {
                name.set("Smithy :: Haskell :: Client :: Codegen")
                description.set(
                    "Smithy codegen plugin for generating Haskell clients.",
                )
                url.set("https://github.com/juspay/smithy-hs")
                licenses {
                    license {
                        name.set("Apache License 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("shrey_bana")
                        name.set("Shrey Bana")
                        email.set("shrey.bana@juspay.in")
                        organization.set("Juspay")
                        organizationUrl.set("https://juspay.io")
                        roles.add("developer")
                    }
                }
                scm {
                    connection.set("https://github.com/juspay/smithy-hs.git")
                    developerConnection.set("https://github.com/juspay/smithy-hs.git")
                    url.set("https://github.com/juspay/smithy-hs.git")
                }
            }
        }
    }
    repositories {
        maven {
            url = uri(rootProject.layout.buildDirectory.dir("m2"))
        }
    }
}
