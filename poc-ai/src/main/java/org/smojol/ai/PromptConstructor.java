package org.smojol.ai;

public class PromptConstructor {
    private StringBuilder builder = new StringBuilder();
    public PromptConstructor addLine(String s) {
        builder.append(s + "\n");
        return this;
    }

    public String getPrompt() {
        return builder.toString();
    }
}
