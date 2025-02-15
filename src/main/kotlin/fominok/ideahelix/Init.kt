package fominok.ideahelix

import clojure.java.api.Clojure
import clojure.lang.IFn
import com.intellij.ide.IdeEventQueue
import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.ProjectActivity
import java.awt.KeyboardFocusManager
import java.awt.event.KeyEvent

class Init : ProjectActivity {
    override suspend fun execute(project: Project) {
        val pushEvent: IFn;

        // Per https://plugins.jetbrains.com/docs/intellij/plugin-class-loaders.html#using-serviceloader:
        val currentThread = Thread.currentThread()
        val originalClassLoader = currentThread.contextClassLoader
        val pluginClassLoader = javaClass.classLoader
        try {
            currentThread.contextClassLoader = pluginClassLoader

            val require = Clojure.`var`("clojure.core", "require")
            require.invoke(Clojure.read("fominok.ideahelix.core"))

            pushEvent = Clojure.`var`("fominok.ideahelix.core", "push-event") as IFn

            val initFn = Clojure.`var`("fominok.ideahelix.core", "init-project") as IFn
            initFn.invoke(project)
        } finally {
            currentThread.contextClassLoader = originalClassLoader

        }

        IdeEventQueue.getInstance().addDispatcher({
            if ((it is KeyEvent) && (it.id == KeyEvent.KEY_TYPED || it.keyCode == KeyEvent.VK_ESCAPE)) {
                val focusOwner = KeyboardFocusManager.getCurrentKeyboardFocusManager().focusOwner

                pushEvent.invoke(project, focusOwner, it) as Boolean
            } else {
                false
            }
        }, project)
    }
}
