import { HttpClientModule } from '@angular/common/http';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { BrowserModule } from '@angular/platform-browser';
import { RouterModule } from '@angular/router';
import { MonacoEditorModule } from 'ngx-monaco-editor';
import { AppComponent } from './app.component';
import { appRoutes } from './app.routes';
import { DynamicFormModule } from './dynamic-form/dynamic-form.module';
import { monacoOnload } from './monaco-onload.function';
import { CobolkitModule } from './shared/cobolkit/cobolkit.module';

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule,
    DynamicFormModule,
    CobolkitModule,
    MonacoEditorModule.forRoot({
      onMonacoLoad: monacoOnload
    }),
    RouterModule.forRoot(appRoutes, { useHash: true }),
  ],
  bootstrap: [AppComponent],
})
export class AppModule { }
