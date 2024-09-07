package org.smojol.toolkit.analysis.defined;

import org.smojol.common.ast.AggregatingFlowNodeASTVisitor;
import org.smojol.common.ast.CommentBlock;
import org.smojol.common.ast.CommentExtraction;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.navigation.CobolEntityNavigator;
import org.smojol.common.navigation.DataStructureNavigator;
import org.smojol.common.navigation.FlowNodeNavigator;
import org.smojol.toolkit.task.CommandLineAnalysisTask;
import org.smojol.toolkit.task.AnalysisTask;
import org.smojol.toolkit.task.AnalysisTaskResult;
import org.smojol.common.vm.structure.CobolDataStructure;
import org.smojol.toolkit.analysis.pipeline.config.SourceConfig;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

public class AttachCommentsTask implements AnalysisTask {
    private static final Logger LOGGER = Logger.getLogger(AttachCommentsTask.class.getName());
    private final FlowNode astRoot;
    private final CobolDataStructure dataStructures;
    private final CobolEntityNavigator navigator;
    private final SourceConfig sourceConfig;

    public AttachCommentsTask(FlowNode astRoot, CobolDataStructure dataStructures, CobolEntityNavigator navigator, SourceConfig sourceConfig) {
        this.astRoot = astRoot;
        this.dataStructures = dataStructures;
        this.navigator = navigator;
        this.sourceConfig = sourceConfig;
    }

    @Override
    public AnalysisTaskResult run() {
        try {
            List<CommentBlock> commentBlocks = new CommentExtraction().run(sourceConfig.sourcePath(), navigator);
            commentBlocks.forEach(cb -> {
                LOGGER.finer("Attaching comments");
                FlowNode node = new FlowNodeNavigator(astRoot).findNarrowestByCondition(n -> n.originalText().contains(cb.getCodeContextLine()));
                if (node != null) node.addComment(cb);
                else {
                    CobolDataStructure dataStructure = new DataStructureNavigator(dataStructures).findByCondition(ds -> ds.getRawText().contains(cb.getCodeContextLine()));
                    if (dataStructure != null) dataStructure.addComment(cb);
                    else {
                        FlowNode possibleIdmsNode = new FlowNodeNavigator(astRoot).findByCondition(n -> n.originalText().contains(cb.getCodeContextLine()));
                        if (possibleIdmsNode != null) possibleIdmsNode.addComment(cb);
                        else astRoot.addComment(cb);
                    }
                }
            });
            return AnalysisTaskResult.OK(CommandLineAnalysisTask.ATTACH_COMMENTS);
        } catch (IOException e) {
            return AnalysisTaskResult.ERROR(e, CommandLineAnalysisTask.ATTACH_COMMENTS);
        }
    }
}
