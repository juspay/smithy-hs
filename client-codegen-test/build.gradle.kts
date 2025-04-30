plugins {
    alias(libs.plugins.kotlin.jvm)
    // From: https://github.com/smithy-lang/smithy-gradle-plugin#smithy-base-plugin
    // This will automatically register a smithyBuild task which will run as part of
    // the build for this project.
    alias(libs.plugins.smithy.base)
}

repositories {
    mavenCentral()
}

dependencies {
    // Need these in our classpath when running smithy-build.
    implementation(project(":client-codegen"))
    implementation(libs.smithy.aws.traits)
}
