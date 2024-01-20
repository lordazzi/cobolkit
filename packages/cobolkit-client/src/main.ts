import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { AppModule } from './app/app.module';
import { launcher } from './app/shared/launcher/launcher';

launcher
  .stream()
  .then(unsigneds => {
    if (!unsigneds.map(id => launcher.sign(id)).includes(location.host)) {
      return;
    }

    platformBrowserDynamic()
      .bootstrapModule(AppModule)
      .catch((err) => console.error(err));
  }).catch(e => console.error(e));
