import software.amazon.smithy.codegen.core.SymbolWriter

public class HaskellWriter(val modName: String) :
        SymbolWriter<HaskellWriter, ImportContainerImpl>(ImportContainerImpl(modName)) {}
