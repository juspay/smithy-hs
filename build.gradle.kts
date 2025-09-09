import org.jreleaser.model.Active

plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.spotless)
    alias(libs.plugins.jreleaser)
}

spotless {
    kotlin {
        // Being specific w/ the `src` dir here as otherwise `spotless` starts breaking
        // for `client-codegen-test`. Essentially `spotless` starts looking at files
        // produced by `smithyBuild` & then gradle complains that there is an implicit
        // dependency. Can be fixed in other-ways, but this is good enough for now.
        target("**/src/**/*.kt")
        // FIXME We should be able to do this .editorconfig as well, but it doesn't seem to be
        // working...
        ktlint().editorConfigOverride(
            mapOf(
                "ktlint_standard_package-name" to "disabled",
                "ktlint_standard_no-wildcard-imports" to "disabled",
            ),
        )
    }
}

allprojects {
    group = "in.juspay.smithy.haskell"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }
}

jreleaser {
    dryrun = false

    // Used for creating a tagged release, uploading files and generating changelog.
    // In the future we can set this up to push release tags to GitHub, but for now it's
    // set up to do nothing.
    // https://jreleaser.org/guide/latest/reference/release/index.html
    release {
        generic {
            enabled = true
            skipRelease = true
        }
    }

    announce {
        active = Active.NEVER
    }

    signing {
        active = Active.ALWAYS
        armored = true
    }

    deploy {
        maven {
            mavenCentral {
                create("maven-central") {
                    active = Active.ALWAYS
                    url = "https://central.sonatype.com/api/v1/publisher"
                    snapshotSupported = true
                    stagingRepository(
                        rootProject.layout.buildDirectory
                            .dir("m2")
                            .get()
                            .asFile.path,
                    )
                }
            }
        }
    }
}
