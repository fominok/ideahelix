plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "2.1.0"
    id("org.jetbrains.intellij.platform") version "2.5.0"
}

import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

group = "fominok"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
    maven("https://repo.clojars.org/")
}

sourceSets {
    main {
        resources {
            srcDirs("src/main/clojure")
        }
    }
}

val aotOutputDir = layout.buildDirectory.dir("generated/aot/main")

val compileClojureBridgeAot by tasks.registering(JavaExec::class) {
    val outputDir = aotOutputDir.get().asFile

    inputs.files(fileTree("src/main/clojure"))
    outputs.dir(outputDir)

    classpath = sourceSets.main.get().compileClasspath + files("src/main/clojure")
    mainClass.set("clojure.main")

    doFirst {
        outputDir.mkdirs()
    }

    args(
        "-e",
        """
        (binding [*compile-path* "${outputDir.absolutePath.replace("\\", "\\\\")}"]
          (compile 'fominok.ideahelix.bridge))
        """.trimIndent(),
    )
}

sourceSets.main {
    output.dir(mapOf("builtBy" to compileClojureBridgeAot), aotOutputDir)
    compileClasspath += files(aotOutputDir)
    runtimeClasspath += files(aotOutputDir)
}

// Configure Gradle IntelliJ Plugin
// Read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin.html
dependencies {
    intellijPlatform {
        create("IC", "2025.1")
        testFramework(org.jetbrains.intellij.platform.gradle.TestFrameworkType.Platform)

        // Add necessary plugin dependencies for compilation here, example:
        // bundledPlugin("com.intellij.java")
    }

    implementation("org.clojure:clojure:1.12.0")

    implementation("io.github.tonsky:toml-clj:0.1.0")

    implementation("nrepl:nrepl:1.3.1")

    implementation("cider:cider-nrepl:0.50.0")

    implementation("org.clojure:spec.alpha:0.5.238")
}

intellijPlatform {
    pluginConfiguration {
        ideaVersion {
            sinceBuild = "251"
        }

        changeNotes = """
      Initial version
    """.trimIndent()
    }
}

tasks {
    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "21"
        targetCompatibility = "21"
    }
    withType<KotlinCompile> {
        dependsOn(compileClojureBridgeAot)
        kotlinOptions.jvmTarget = "21"
    }
}
