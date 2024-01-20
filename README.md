# Bem vindo a manutenção do Cobolkit

[os comandos de criação de arquivos e pastas exemplificados neste tutorial se aplicam ao sistema operacional ubuntu]

Esta é uma aplicação client-server, o workspace foi gerado com [npx/nx](https://nx.dev/), e foi incluso neste workspace uma aplicação [angular](https://angular.io/docs) e uma [nestjs](https://docs.nestjs.com/) que serve via api a lógica da biblioteca cobol-driver. O projeto é configurado com [typeScript strict](https://www.typescriptlang.org/tsconfig#strict) e um [linter](https://eslint.org/) reforçado que garante uma distribuição da complexidade do sistema em classes e métodos com responsabilidades mais bem definidas, aumentando o reaproveitamento de código e a qualidade da manutenção futura.

A criação da estrutura base deste projeto se deu com a execução dos seguintes comandos:

1. Instalação do npx na máquina host: ```npm install -g npx nx```
2. Criação do workspace: ```npx create-nx-workspace cobolkit-reborn --preset=ts```
3. Direcionamento ao repositório: ```git remote set-url origin [url]```
4. Instalação da estrutura client-server com angular e nest:
```npm install --save-dev @nrwl/angular @nrwl/nest @nrwl/js;```
```npx nx g @nrwl/angular:app cobolkit-client;```
```npx nx g @nrwl/nest:app cobolkit-server;```
```npx nx g @nrwl/js:lib cobol-driver;```


## Ambiente de desenvolvimento

### Dependências
Está configurado no projeto um ambiente de desenvolvimento dockermizado, composto pelos arquivos `docker-compose.yml`, `Dockerfile.dev` e a pasta `.devcontainer`. Para fazer uso dele é necessários instalar  o [docker](https://docs.docker.com/engine/install/), o [docker-compose](https://www.digitalocean.com/community/tutorials/how-to-install-docker-compose-on-ubuntu-18-04-pt) e incluir em seu vscode a extenção [remote containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) da Microsoft, ou então a [remote explorer](https://marketplace.visualstudio.com/items?itemName=ms-vscode.remote-explorer), permitindo abrir um vscode customizado com as extenções que compõe o ambiente de desenvolvimento do projeto, assim como um sistema operacional com os pacotes e configurações customizados para o projeto.

Instale o [https://code.visualstudio.com/docs/devcontainers/devcontainer-cli](devcontainer cli) para inicializar o vscode customizado através da linha de comando. Se você fizer a instalação através do ```npm``` ele não será capaz de usar o comando ```devcontainer open .``` para inicializar o ambiente, portanto recomendo que faça a instalação pressionando ```Ctrl + Shift + P``` no vscode e buscando por "Dev Containers: Install devcontainer CLI".

### Autenticação do ambiente com o repositório
#### sobre o workspace
Apesar da base desta aplicação ser construída por uma biblioteca de geração de workspace, o projeto não deve ser encarado como workspace, mas como uma aplicação client-server e este projeto deve ficar dentro de uma outra pasta que será realmente o workspace, minha recomendação de organização: ```/workspace/cobolkit/cobolkit-reborn```, onde ```workspace``` é uma pasta raíz onde incluo todos meus projetos de todas as linguagens, ```cobolkit``` é a pasta que inclui as customizações para os projetos relacionados ao cobolkit e ```cobolkit-reborn``` é o projeto criado através do gerado de workspace [npx/nx](https://nx.dev/);

#### autenticação no bitbucket na pasta do workspace
1. Um arquivo .gitconfig e uma pasta .ssh devem ser criados e devem receber permissão 777, para isso, na pasta workspace cobolkit, execute os comandos:
```touch .gitconfig```
```mkdir .ssh```
```chmod 777 .gitconfig```
```chmod 777 .ssh```

2. colocaremos agora chaves ssh novas na pasta e o conteúdo gerado no arquivo .pub deve ser copiado e colado em sua coleção de chaves de autenticidade do bitbucket (imagem de perfil > personal settings > ssh keys), comando para gerar chave e arquivo .pub: ```ssh-keygen -C "seu.email.bitbucket@email.com.br"```

### Instalação
1. Crie a pasta node_modules (```mkdir node_modules```) e ceagamode ela em 777 (```chmod 777 -R node_modules```), isso dará permissão do usuário interno do SO do ambiente de desenvolvimento de manipular o conteúdo da pasta;
2. Execute ```docker-compose build```, e a imagem docker será baixada e o node_modules será instalado;
3. Execute ```devcontainer build``` e as extenções de vscode serão baixadas na imagem docker;
4. Inicialize o ambiente com ```devcontainer open .``` e vscode do projeto será inicializado e o conteúdo no node_modules de sua máquina host será preenchido;
5. Acessando o ambiente docker com o vscode, registre seu usuário git com os seguintes comandos:
```
git config --global user.name "Nome Sobrenome"
git config --global user.email nome.sobrenome@email.com.br
```

## Criação de código

Ao invés da linha de comando do angular ou do nest, o nx é usado para gerar componentes e serviços em ambos os casos, exemplo:

Execute `nx generate component component-folder/component-name` dentro da pasta do projeto para gerar um novo componente, podendo se abreviar com `nx g c component-folder/component-name`. Da mesma forma usará para criar directive, pipe, service, guard, module`. [Saiba mais](https://nx.dev/packages/angular/generators/component).

Execute `nx generate service service-folder/service-name` dentro da pasta do projeto para gerar um novo serviço nextjs, podendo se abreviar com `nx g s service-folder/service-name`. [Saiba mais](https://nx.dev/packages/nest/generators/service).

## Build
Execute `npm run build` para compilar o projeto. O conteúdo do build será armazenado na pasta `dist/`.

## Mais ajuda?
- [Angular CLI Overview and Command Reference](https://angular.io/cli)
- [Angular Styleguide](https://angular.io/guide/styleguide)
- [Rxjs](https://rxjs.dev/guide/overview)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [ES6 Features](http://es6-features.org/)
- [Docker Compose](https://docs.docker.com/compose/)
- [Code Devcontainer Pre-Build](https://code.visualstudio.com/docs/remote/containers#_quick-start-open-an-existing-folder-in-a-container)
- [Code Devcontainer CLI](https://code.visualstudio.com/docs/remote/devcontainer-cli)
- [Design Patterns](https://refactoring.guru/pt-br/design-patterns/creational-patterns)