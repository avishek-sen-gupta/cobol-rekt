package org.smojol.toolkit.analysis.defined;

import com.google.common.collect.ImmutableList;
import org.antlr.v4.runtime.tree.ParseTree;
import org.smojol.common.ast.*;
import org.smojol.common.dependency.ComponentsBuilder;
import org.smojol.common.dialect.LanguageDialect;
import org.smojol.common.flowchart.FlowchartBuilder;
import org.smojol.common.id.UUIDProvider;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.EntityNavigatorBuilder;
import org.smojol.common.pseudocode.*;
import org.smojol.common.vm.interpreter.Breakpointer;
import org.smojol.common.vm.interpreter.ExecutionListeners;
import org.smojol.common.vm.interpreter.FlowControl;
import org.smojol.common.vm.strategy.UnresolvedReferenceDoNothingStrategy;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.ParsePipeline;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;
import org.smojol.toolkit.ast.DisplayFlowNode;
import org.smojol.toolkit.ast.FlowchartBuilderImpl;
import org.smojol.toolkit.interpreter.interpreter.CobolBreakpointer;
import org.smojol.toolkit.interpreter.interpreter.CobolConditionResolver;
import org.smojol.toolkit.interpreter.interpreter.CobolInterpreterFactory;
import org.smojol.toolkit.interpreter.navigation.FlowNodeASTTraversal;
import org.smojol.toolkit.interpreter.structure.DefaultFormat1DataStructureBuilder;
import org.smojol.toolkit.task.*;

import java.io.IOException;

public class InterpretTask implements AnalysisTask {
    private static final String INTERPRET_SOURCE = "INTERPRET_SOURCE";
    private final SourceConfig sourceConfig;
    private final LanguageDialect dialect;

    public InterpretTask(SourceConfig sourceConfig, LanguageDialect dialect) {
        this.sourceConfig = sourceConfig;
        this.dialect = dialect;
    }

    @Override
    public AnalysisTaskResult run() {
        ComponentsBuilder ops = new ComponentsBuilder(new CobolTreeVisualiser(),
                FlowchartBuilderImpl::build, new EntityNavigatorBuilder(), new UnresolvedReferenceDoNothingStrategy(),
                new DefaultFormat1DataStructureBuilder(), new UUIDProvider());
        ParsePipeline pipeline = new ParsePipeline(sourceConfig, ops, dialect);
        try {
            CobolEntityNavigator navigator = pipeline.parse();
            FlowchartBuilder flowcharter = pipeline.flowcharter();
            CobolDataStructure dataStructures = pipeline.getDataStructures();
            ParseTree procedure = navigator.procedureBodyRoot();
            flowcharter.buildFlowAST(procedure).buildControlFlow().buildOverlay();
            FlowNode root = flowcharter.getRoot();
            new FlowNodeASTTraversal<FlowNode>().accept(root, new FlowNodeSymbolExtractorVisitor(root, new SmojolSymbolTable(dataStructures, new SymbolReferenceBuilder(new UUIDProvider())), dataStructures));
            System.out.println("DATA STRUCTURES\n--------------------------------\n");
            dataStructures.report();
            System.out.println("INTERPRETING\n--------------------------------\n");
            Breakpointer bp = new CobolBreakpointer();
            bp.addBreakpoint(n -> n.getClass() == DisplayFlowNode.class && n.originalText().contains("SOMETEXT"));
            root.acceptInterpreter(CobolInterpreterFactory.executingInterpreter(CobolConditionResolver.EVALUATING_RESOLVER, dataStructures, ImmutableList.of(), new ExecutionListeners(ImmutableList.of()), bp), FlowControl::CONTINUE);
            return new AnalysisTaskResultOK(INTERPRET_SOURCE);
        } catch (IOException e) {
            return new AnalysisTaskResultError(e, INTERPRET_SOURCE);
        }
    }
}
