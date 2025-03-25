import software.amazon.smithy.codegen.core.ImportContainer
import software.amazon.smithy.codegen.core.Symbol

public class ImportContainerImpl(val modName: String) : ImportContainer {
    override fun importSymbol(sym: Symbol, name: String) {}
}
