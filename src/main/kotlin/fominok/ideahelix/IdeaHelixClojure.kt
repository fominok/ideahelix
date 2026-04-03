package fominok.ideahelix

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import java.awt.event.KeyEvent

object IdeaHelixClojure {
    private fun <T> withPluginClassLoader(action: () -> T): T {
        val currentThread = Thread.currentThread()
        val originalClassLoader = currentThread.contextClassLoader
        val pluginClassLoader = javaClass.classLoader

        return try {
            currentThread.contextClassLoader = pluginClassLoader
            action()
        } finally {
            currentThread.contextClassLoader = originalClassLoader
        }
    }

    fun pushEditorEvent(project: Project, editor: Editor, event: KeyEvent): Boolean {
        val handled = withPluginClassLoader {
            Bridge.pushEditorEvent(project, editor, event)
        }
        updateModeWidget(project)
        return handled
    }

    fun focusEditor(project: Project, editor: Editor) {
        withPluginClassLoader {
            Bridge.focusEditor(project, editor)
        }
        updateModeWidget(project)
    }

    fun releaseEditor(project: Project, editor: Editor) {
        withPluginClassLoader {
            Bridge.releaseEditor(project, editor)
        }
        updateModeWidget(project)
    }

    fun currentMode(project: Project): String =
        withPluginClassLoader {
            Bridge.currentMode(project)
        }

    fun currentModeDisplay(project: Project): String =
        withPluginClassLoader {
            Bridge.currentModeDisplay(project)
        }

    fun updateModeWidget(project: Project) {
        ModeWidget.update(project, currentModeDisplay(project))
    }
}
