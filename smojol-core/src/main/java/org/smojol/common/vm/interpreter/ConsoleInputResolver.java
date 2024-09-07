package org.smojol.common.vm.interpreter;

import org.eclipse.lsp.cobol.core.CobolParser;
import org.smojol.common.ast.FlowNode;
import org.smojol.common.ast.FlowNodeService;
import org.smojol.common.flowchart.ConsoleColors;
import org.smojol.common.ast.NodeText;

import java.util.Scanner;
import java.util.logging.Logger;

public class ConsoleInputResolver implements BooleanResolver {
    java.util.logging.Logger LOGGER = Logger.getLogger(ConsoleInputResolver.class.getName());
    public static BooleanResolver CONSOLE_INPUT_RESOLVER = new ConsoleInputResolver();
    @Override
    public boolean resolve(FlowNode node, CobolParser.ConditionContext condition, FlowNodeService nodeService) {
        Scanner scanner = new Scanner(System.in);
        LOGGER.info(ConsoleColors.coloured("Waiting for condition " + NodeText.formatted(node.label()) + ": ", 208));
        String trueOrFalse = scanner.nextLine().trim().toUpperCase();
        return "Y".equals(trueOrFalse);
    }
}
