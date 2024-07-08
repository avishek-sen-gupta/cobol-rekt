package org.smojol.common.id;

import java.util.UUID;

public class UUIDProvider implements IdProvider {
    @Override
    public String next() {
        return UUID.randomUUID().toString();
    }
}
