package fominok.ideahelix

import clojure.java.api.Clojure
import clojure.lang.IFn
import clojure.lang.Keyword
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import java.awt.event.KeyEvent

object IdeaHelixClojure {
    @Volatile
    private var loaded = false

    private lateinit var pushEditorEventFn: IFn
    private lateinit var focusEditorFn: IFn
    private lateinit var currentModeFn: IFn

    @Synchronized
    private fun ensureLoaded() {
        if (loaded) return

        val currentThread = Thread.currentThread()
        val originalClassLoader = currentThread.contextClassLoader
        val pluginClassLoader = javaClass.classLoader

        try {
            currentThread.contextClassLoader = pluginClassLoader

            val require = Clojure.`var`("clojure.core", "require")
            require.invoke(Clojure.read("fominok.ideahelix.core"))

            pushEditorEventFn = Clojure.`var`("fominok.ideahelix.core", "push-editor-event")
            focusEditorFn = Clojure.`var`("fominok.ideahelix.core", "focus-editor")
            currentModeFn = Clojure.`var`("fominok.ideahelix.core", "current-mode")
            loaded = true
        } finally {
            currentThread.contextClassLoader = originalClassLoader
        }
    }

    fun pushEditorEvent(project: Project, editor: Editor, event: KeyEvent): Boolean {
        ensureLoaded()
        return pushEditorEventFn.invoke(project, editor, event) as Boolean
    }

    fun focusEditor(project: Project, editor: Editor) {
        ensureLoaded()
        focusEditorFn.invoke(project, editor)
    }

    fun currentMode(project: Project): String {
        ensureLoaded()
        return when (val value = currentModeFn.invoke(project)) {
            is Keyword -> value.name
            null -> "normal"
            else -> value.toString()
        }
    }
}
