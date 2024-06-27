/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.smojol.analysis.visualisation;

import com.google.common.collect.ImmutableList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.smojol.flowchart.FlowchartTasks;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class FlowchartBuildMain {
    private final Logger logger = LoggerFactory.getLogger(FlowchartBuildMain.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        String sourceDir = "/Users/asgupta/Downloads/mbrdi-poc";
        File[] copyBookPaths = new File[]{new File("/Users/asgupta/Downloads/mbrdi-poc")};
        String dialectJarPath = "/Users/asgupta/code/mbrdi-proleap/che4z/che-che4z-lsp-for-cobol-2.1.2/server/dialect-idms/target/dialect-idms.jar";
        String reportRootDir = "/Users/asgupta/Downloads/mbrdi-poc/report";

        List<String> programNames = ImmutableList.of("V751C931", "V7588049", "V75234");
//        List<String> programNames = ImmutableList.of("V75234");
        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).generateForPrograms(programNames);
//        new FlowchartTasks(sourceDir, reportRootDir, copyBookPaths, dialectJarPath).singleFlowchartDemo();
    }
}
