import { createApp } from 'vue'
import App from './App.vue'
import PrimeVue from 'primevue/config';
import Aura from '@primevue/themes/aura';
import Button from "primevue/button"
import Textarea from 'primevue/textarea';

let app = createApp(App);
app.use(PrimeVue, {
    theme: {
        preset: Aura
    }
});

app.component('LeButton', Button);
app.component('LeTextArea', Textarea);
app.mount('#app')
