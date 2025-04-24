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
        toolVersion = detektV // Keep in sync with plugin version
        config.setFrom(files("$rootDir/detekt.yml"))
        buildUponDefaultConfig = true // Adds default rules to your custom config
        autoCorrect = true // Automatically fixes issues when possible
        parallel = true // Improves performance on multicore machines
    }

    dependencies {
        detektPlugins(detektFormatter)
    }

    // Configure reports on the task level instead
    tasks.withType<io.gitlab.arturbosch.detekt.Detekt>().configureEach {
        config.setFrom(files("$rootDir/detekt.yml"))
        reports {
            html.required.set(true)
            xml.required.set(false)
            txt.required.set(false)
            sarif.required.set(true)
        }
    }
}

// Configure detekt to run as part of the check task
tasks.named("check") { dependsOn(tasks.named("detekt")) }

// Create a specific task just for fixing issues
tasks.register<io.gitlab.arturbosch.detekt.Detekt>("detektFix") {
    description = "Fix Detekt issues."
    setSource(files("src/main/kotlin", "src/test/kotlin"))
    config.setFrom(files("$rootDir/detekt.yml"))
    buildUponDefaultConfig = true
    parallel = true
    autoCorrect = true // Ensure auto-correct is enabled
    disableDefaultRuleSets = false
    debug = true

    reports {
        html.required.set(true)
    }
}
