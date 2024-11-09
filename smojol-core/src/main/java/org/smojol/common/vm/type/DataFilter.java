package org.smojol.common.vm.type;

public interface DataFilter {
    DataFilter NO_FILTER = new DataFilter() {
        @Override
        public String filter(String s) {
            return s;
        }

        @Override
        public int sizeInBytes() {
            return 0;
        }
    };

    String filter(String s);
    int sizeInBytes();
}
