package org.smojol.toolkit.analysis.task.interpret;

import com.google.common.collect.ImmutableList;
import com.mojo.algorithms.id.UUIDProvider;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.CobolTreeVisualiser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.pseudocode.SmojolSymbolTable;
import org.smojol.common.pseudocode.SymbolReferenceBuilder;
import org.smojol.common.resource.ResourceOperations;
import org.smojol.common.vm.interpreter.Breakpointer;
import org.smojol.common.vm.interpreter.ConditionResolver;
import org.smojol.common.vm.interpreter.ExecutionListeners;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.ast.BuildFlowNodesTask;
import org.smojol.toolkit.ast.DisplayFlowNode;
import org.smojol.toolkit.ast.FlowNodeServiceImpl;
import org.smojol.toolkit.interpreter.interpreter.CobolBreakpointer;
import org.smojol.toolkit.interpreter.interpreter.CobolInterpreterFactory;
import org.smojol.toolkit.interpreter.interpreter.RunLogger;
import org.smojol.toolkit.interpreter.structure.DefaultFormat1DataStructureBuilder;
import com.mojo.algorithms.task.AnalysisTask;
import com.mojo.algorithms.task.AnalysisTaskResult;
import com.mojo.algorithms.task.AnalysisTaskResultError;
import com.mojo.algorithms.task.AnalysisTaskResultOK;

import java.io.IOException;
import java.util.logging.Logger;

public class InterpretTask implements AnalysisTask {
    private static final java.util.logging.Logger LOGGER = Logger.getLogger(InterpretTask.class.getName());
    private static final String INTERPRET_SOURCE = "INTERPRET_SOURCE";
    private final SourceConfig sourceConfig;
    private final LanguageDialect dialect;
    private final ConditionResolver conditionResolver;
    private final ResourceOperations resourceOperations;

    public InterpretTask(SourceConfig sourceConfig, LanguageDialect dialect, ConditionResolver conditionResolver, ResourceOperations resourceOperations) {
        this.sourceConfig = sourceConfig;
        this.dialect = dialect;
        this.conditionResolver = conditionResolver;
        this.resourceOperations = resourceOperations;
    }

    @Override
    public AnalysisTaskResult run() {
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new DefaultFormat1DataStructureBuilder(), new UUIDProvider(), resourceOperations);
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        try {
            CobolEntityNavigator navigator = pipeline.parse();
            CobolDataStructure dataStructures = pipeline.getDataStructures();
            ParseTree procedure = navigator.procedureDivisionBody(navigator.getRoot());
            FlowNode root = new BuildFlowNodesTask(new FlowNodeServiceImpl(navigator, dataStructures, ops.getIdProvider())).run(procedure);
//            flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
//            FlowNode root = flowcharter.getRoot();
            root.resolve(new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(ops.getIdProvider())), dataStructures);

//            new FlowNodeASTTraversal<FlowNode>().accept(sourceGraphRoot, new FlowNodeSymbolExtractorVisitor(sourceGraphRoot, dataStructures, new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(new UUIDProvider()))));
            LOGGER.info("DATA STRUCTURES\n--------------------------------\n");
            dataStructures.report();
            LOGGER.info("INTERPRETING\n--------------------------------\n");
            Breakpointer bp = new CobolBreakpointer();
            bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class && n.originalText().contains("SOMETEXT"));
            root.acceptInterpreter(CobolInterpreterFactory.executingInterpreter(conditionResolver, dataStructures, ImmutableList.of(), new ExecutionListeners(ImmutableList.of(new RunLogger("/Users/asgupta/code/smojol/out/report.md"))), bp), FlowControl::CONTINUE);
            return new AnalysisTaskResultOK(INTERPRET_SOURCE);
        } catch (IOException e) {
            return new AnalysisTaskResultError(e, INTERPRET_SOURCE);
        }
    }
}
