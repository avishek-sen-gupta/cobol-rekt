package org.smojol.ai;

import com.azure.ai.openai.OpenAIClient;
import com.azure.ai.openai.OpenAIClientBuilder;
import com.azure.ai.openai.models.*;
import com.azure.core.credential.AzureKeyCredential;

import java.util.ArrayList;
import java.util.List;

public class Advisor {
    public static final String AZURE_OPENAI_ENDPOINT = "AZURE_OPENAI_ENDPOINT";
    public static final String AZURE_OPENAI_API_KEY = "AZURE_OPENAI_API_KEY";
    private final String azureOpenaiApiKey;
    private final String azureOpenaiEndpoint;
    private final OpenAIClient client;
    private final String deploymentOrModelId = "gpt35";

    public Advisor(String azureOpenaiApiKey, String azureOpenaiEndpoint) {
        this.azureOpenaiApiKey = azureOpenaiApiKey;
        this.azureOpenaiEndpoint = azureOpenaiEndpoint;
        client = new OpenAIClientBuilder()
                .endpoint(azureOpenaiEndpoint)
                .credential(new AzureKeyCredential(azureOpenaiApiKey))
                .buildClient();
    }

    public List<String> advise(String prompt) {
        List<ChatMessage> prompt2 = new ArrayList<>();
        prompt2.add(new ChatMessage(ChatRole.USER, prompt));

        ChatCompletions completions = client.getChatCompletions(deploymentOrModelId, new ChatCompletionsOptions(prompt2));

        System.out.printf("Model ID=%s is created at %s.%n", completions.getId(), completions.getCreatedAt());
        for (ChatChoice choice : completions.getChoices()) {
            System.out.printf("Index: %d, Text: %s.%n", choice.getIndex(), choice.getMessage().getContent());
        }

        List<String> responses = completions.getChoices().stream().map(c -> c.getMessage().getContent()).toList();

        CompletionsUsage usage = completions.getUsage();
        System.out.printf("Usage: number of prompt token is %d, "
                        + "number of completion token is %d, and number of total tokens in request and response is %d.%n",
                usage.getPromptTokens(), usage.getCompletionTokens(), usage.getTotalTokens());
        return responses;
    }
}
