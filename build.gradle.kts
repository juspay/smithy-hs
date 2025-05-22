// In your root build.gradle.kts file
plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.detekt)
}

val detektV = libs.versions.detekt.get()
val detektFormatter = libs.detekt.formatting

allprojects {
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }

    apply(plugin = "io.gitlab.arturbosch.detekt")

    detekt {
        toolVersion = detektV
        config.setFrom(files("$rootDir/detekt.yml"))
        buildUponDefaultConfig = true // Adds default rules to your custom config
        autoCorrect = true
        parallel = true
    }

    dependencies {
        detektPlugins(detektFormatter)
    }

    tasks.withType<io.gitlab.arturbosch.detekt.Detekt>().configureEach {
        config.setFrom(files("$rootDir/detekt.yml"))
        reports {
            html.required.set(false)
            xml.required.set(false)
            txt.required.set(false)
            sarif.required.set(false)
        }
        // Ignoring errors as this will fail the build command.
        ignoreFailures = true
    }

    // Use this if you want to fail on errors.
    tasks.register<io.gitlab.arturbosch.detekt.Detekt>("detektLint") {
        setSource(files("src/main/kotlin", "src/test/kotlin"))
        config.setFrom(files("$rootDir/detekt.yml"))
        debug = false
        ignoreFailures = false
        reports {
            html.required.set(false)
            xml.required.set(false)
            txt.required.set(false)
            sarif.required.set(false)
        }
        include("**/*.kt")
        include("**/*.kts")
        exclude("resources/")
        exclude("build/")
    }
}
