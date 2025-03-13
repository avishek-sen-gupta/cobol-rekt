package org.smojol.toolkit.examples.architecture;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.mojo.woof.GraphSDK;
import com.mojo.woof.Neo4JDriverBuilder;
import com.mojo.woof.NodeSpec;
import com.mojo.woof.WoofNode;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.neo4j.driver.Record;

import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class ArchitectureMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        Map<String, String> clusterData = clusterInfo();
        List<Pair<String, String>> systemEntries = ImmutableList.of(
                ImmutablePair.of("VDOK", "/Users/asgupta/Downloads/VDOK-F-VAN Questionnaire_Answers.xlsx - Q2.csv"),
                ImmutablePair.of("VAR", "/Users/asgupta/Downloads/Temp MBRDI - VAR-VAN.csv"),
                ImmutablePair.of("WKAL", "/Users/asgupta/Downloads/WKAL Questionnaire_Answers.xlsx - Q2.csv"),
                ImmutablePair.of("TM", "/Users/asgupta/Downloads/Temp MBRDI - TM.csv"),
                ImmutablePair.of("TBE-VAN", "/Users/asgupta/Downloads/Temp MBRDI - TBE-VAN.csv"),
                ImmutablePair.of("TADS-VAN", "/Users/asgupta/Downloads/Temp MBRDI - TADS-VAN.csv"),
                ImmutablePair.of("SKB", "/Users/asgupta/Downloads/Temp MBRDI - SKB.csv"),
                ImmutablePair.of("PW2000", "/Users/asgupta/Downloads/Temp MBRDI - PW2000.csv"),
                ImmutablePair.of("PURAS", "/Users/asgupta/Downloads/Temp MBRDI - PURAS-VAN.csv"),
                ImmutablePair.of("PRO", "/Users/asgupta/Downloads/Temp MBRDI - PRO.csv"),
                ImmutablePair.of("PLAUSI-VAN", "/Users/asgupta/Downloads/Temp MBRDI - PLAUSI-VAN.csv"),
                ImmutablePair.of("NA2000-VAN", "/Users/asgupta/Downloads/Temp MBRDI - NA2000-VAN.csv"),
                ImmutablePair.of("MDA", "/Users/asgupta/Downloads/Temp MBRDI - MDA.csv"),
                ImmutablePair.of("MAS", "/Users/asgupta/Downloads/Temp MBRDI - MAS.csv"),
                ImmutablePair.of("LTS", "/Users/asgupta/Downloads/Temp MBRDI - LTS.csv"),
                ImmutablePair.of("FRP", "/Users/asgupta/Downloads/Temp MBRDI - FRP.csv"),
                ImmutablePair.of("FRE", "/Users/asgupta/Downloads/Temp MBRDI - FRE.csv"),
                ImmutablePair.of("SL2000", "/Users/asgupta/Downloads/Temp MBRDI - SL2000.csv"),
                ImmutablePair.of("EW2000", "/Users/asgupta/Downloads/Temp MBRDI - EW2000.csv"),
                ImmutablePair.of("GP2000", "/Users/asgupta/Downloads/Temp MBRDI - GP2000.csv"),
                ImmutablePair.of("DDA", "/Users/asgupta/Downloads/Temp MBRDI - DDA.csv"),
                ImmutablePair.of("FAV", "/Users/asgupta/Downloads/Temp MBRDI - FAV.csv"),
                ImmutablePair.of("DiVerS", "/Users/asgupta/Downloads/Temp MBRDI - DiVerS.csv"),
                ImmutablePair.of("VANIS", "/Users/asgupta/Downloads/Temp MBRDI - VANIS.csv")
        );
        List<RawSystemConnectionEntry> allRawConnectionEntries = systemEntries.stream().map(ArchitectureMain::systemEntries).reduce(Stream::concat).get().toList();

        List<String> uniqueCurrentSystemNames = Stream.concat(allRawConnectionEntries.stream()
                        .map(RawSystemConnectionEntry::adjacentSystemName), allRawConnectionEntries.stream().map(RawSystemConnectionEntry::currentSystemName))
                .distinct().toList();

        Map<String, LegacySystem> uniqueSystems = uniqueCurrentSystemNames.stream()
                .map(systemName -> new LegacySystem(systemName, allRawConnectionEntries.stream()
                        .filter(sce -> sce.adjacentSystemName().equals(systemName))
                        .map(RawSystemConnectionEntry::systemType)
                        .toList(), cluster(systemName, clusterData)))
                .collect(Collectors.toUnmodifiableMap(
                        LegacySystem::systemName,
                        o -> o
                ));
        List<SystemConnectionEntry> systemConnectionEntries = allRawConnectionEntries.stream().map(rce -> systemEntry(rce, uniqueSystems)).toList();
        GraphSDK sdk = new GraphSDK(new Neo4JDriverBuilder().fromEnv());
        uniqueSystems.values().forEach(system -> {
            sdk.createNode(new WoofNode(ImmutableMap.of("name", system.systemName(), "technologies", techsAsStrings(system), "cluster", system.clusterName()),
                    Stream.concat(Stream.of("SYSTEM", system.clusterName()), techsAsStrings(system).stream()).toList()));
        });
        systemConnectionEntries.forEach(sce ->
        {
            Record systemNode = sdk.findNodes(new NodeSpec(ImmutableList.of(), ImmutableMap.of("name", sce.currentSystem().systemName()))).getFirst();
            Record adjacentSystemNode = sdk.findNodes(new NodeSpec(ImmutableList.of(), ImmutableMap.of("name", sce.adjacentSystem().systemName()))).getFirst();
            if (sce.connectionDirection() == ConnectionDirection.OUT)
                sdk.connect(systemNode, adjacentSystemNode, "ACCESSES", "SYSTEM_CONNECTION");
            else
                sdk.connect(adjacentSystemNode, systemNode, "ACCESSES", "SYSTEM_CONNECTION");
        });
//        System.out.println("Unique currentSystemNames: " + uniqueSystems.keySet());

        List<String> clusterAppNames = clusterData.entrySet().stream().filter(e -> e.getValue().equals("AFAB")).map(x -> x.getKey()).toList();
        List<LegacySystem> clusterSystems = uniqueSystems.values().stream().filter(sys -> clusterAppNames.contains(sys.systemName())).toList();

        List<Pair<LegacySystem, List<Pair<String, List<LegacySystem>>>>> clusterPartitionedDependenciesByApp = clusterSystems.stream().map(system -> {
            Record systemNode = sdk.findNodes(new NodeSpec(ImmutableList.of(), ImmutableMap.of("name", system.systemName()))).getFirst();
//            System.out.println("ANALYSING SYSTEM: " + system.systemName());
            List<LegacySystem> unclassifiedChildren = sdk.directChildrenAnyDirection(systemNode, "ACCESSES")
                    .stream().map(adjacentSystem -> {
//                        System.out.println("Looking for: " + adjacentSystem.get("n").get("name"));
                        LegacySystem foundSystem = uniqueSystems.get(adjacentSystem.get("n").get("name").asString());
//                        if (foundSystem == null) System.out.println("Could not find system: " + adjacentSystem.get("n").get("name"));
                        return foundSystem;
                    }).toList();
//            unclassifiedChildren.forEach(uc -> System.out.println("UNCLASSIFIED SYSTEM: " + uc.systemName()));
            List<Pair<String, List<LegacySystem>>> x = clusterData.values().stream()
                    .distinct().map(clusterName -> Pair.of(clusterName, unclassifiedChildren.stream()
                            .filter(c -> clusterName.equals(c.clusterName())).toList())).toList();
            return Pair.of(system, x);
        }).toList();
        System.out.println(clusterPartitionedDependenciesByApp);

        clusterPartitionedDependenciesByApp.forEach(p -> {
            System.out.println("Dependencies for app: " + p.getLeft());
            p.getRight().forEach(partition -> {
                System.out.println("Partition: " + partition.getLeft());
                partition.getRight().forEach(partitionedSystem -> System.out.println(partitionedSystem.systemName()));
                if (partition.getRight().isEmpty()) System.out.println("No apps in this partition");
                System.out.println("--------------------------");
            });
            System.out.println("===============================");
        });
    }

    private static String cluster(String systemName, Map<String, String> clusterData) {
        if (!clusterData.containsKey(systemName)) return "UNKNOWN";
        return clusterData.get(systemName);
    }

    private static Map<String, String> clusterInfo() {
        Map<String, String> clusterInfo = new HashMap<>();
        clusterInfo.put("AFAB", "AFAB");
        clusterInfo.put("CORFU", "AFAB");
        clusterInfo.put("EW2000", "AFAB");
        clusterInfo.put("GP2000", "AFAB");
        clusterInfo.put("LTS", "AFAB");
        clusterInfo.put("PW2000", "AFAB");
        clusterInfo.put("SL2000", "AFAB");
        clusterInfo.put("TM", "AFAB");
        clusterInfo.put("WKAL", "AFAB");
        clusterInfo.put("I-CUST", "AFAB");
        clusterInfo.put("MDC", "AFAB");
        clusterInfo.put("NA2000-VAN", "AFAB");
        clusterInfo.put("PLAUSI-VAN", "AFAB");
        clusterInfo.put("VDOK", "AFAB");
        clusterInfo.put("TADS-VAN", "AFAB");
        clusterInfo.put("DDA", "Finance");
        clusterInfo.put("FAV", "Finance");
        clusterInfo.put("FRE", "Finance");
        clusterInfo.put("FRP", "Finance");
        clusterInfo.put("MDA", "Finance");
        clusterInfo.put("PRO", "Finance");
        clusterInfo.put("SKB", "Finance");
        clusterInfo.put("MAS", "Custom");
        clusterInfo.put("TBE", "VANIS");
        clusterInfo.put("VANIS", "VANIS");
        clusterInfo.put("PURAS", "Different_BU");
        clusterInfo.put("VAR", "Different_BU");
        clusterInfo.put("DUMMY", "UNKNOWN");
        return clusterInfo;
    }

    private static List<String> techsAsStrings(LegacySystem system) {
        if (system.technologies().isEmpty()) return ImmutableList.of("NONE");
        return system.technologies().stream().map(SystemType::toLabel).toList();
    }

    private static SystemConnectionEntry systemEntry(RawSystemConnectionEntry rce, Map<String, LegacySystem> uniqueSystems) {
        LegacySystem currentSystem = uniqueSystems.get(rce.currentSystemName());
        LegacySystem adjacentSystem = uniqueSystems.get(rce.adjacentSystemName());
        return new SystemConnectionEntry(currentSystem, adjacentSystem, rce.connectionDirection(), rce.connectionNature(), rce.latency(), rce.usagePattern());
    }

    private static Stream<RawSystemConnectionEntry> systemEntries(Pair<String, String> systemEntry) {
        try {
            FileReader in = new FileReader(systemEntry.getRight());
            Iterable<CSVRecord> records = CSVFormat.RFC4180.builder()
                    .setHeader()
                    .setSkipHeaderRecord(true)
                    .get()
                    .parse(in);
            return StreamSupport.stream(records.spliterator(), false)
                    .map(rec -> new RawSystemConnectionEntry(systemEntry.getLeft(),
                            rec.get(0).trim(),
                            connectionDirection(rec.get(1).trim()),
                            new SystemType(rec.get(2).trim()),
                            connectionNature(rec.get(3).trim()),
                            new Latency(rec.get(4).trim()),
                            new UsagePattern(rec.get(5).trim())));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static ConnectionDirection connectionDirection(String connectionDirectionAsString) {
        if (!"OUT".equals(connectionDirectionAsString) && !"IN".equals(connectionDirectionAsString))
            return ConnectionDirection.UNKNOWN;
        return ConnectionDirection.valueOf(connectionDirectionAsString);
    }

    private static ConnectionNature connectionNature(String connectionNature) {
        return "asynchron".equals(connectionNature) ? ConnectionNature.ASYNCHRONOUS : ConnectionNature.SYNCHRONOUS;
    }
}
