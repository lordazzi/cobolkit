# Ambiente de desenvolvimento light, exclusivamente com o necessário
FROM node:16.3.0

ENV LANG=C.UTF-8
ENV TZ America/Sao_Paulo

# Creating workdir
RUN mkdir /cobolkit && chmod 777 /cobolkit
WORKDIR /cobolkit

# Open door
EXPOSE 4200
EXPOSE 8091

# Setting configs
RUN git config --global core.editor 'code --wait'
RUN mkdir /root/.ssh

# Installing dev dependencies
RUN npm install nx@latest @angular-devkit/build-angular@latest -g
COPY package.json /cobolkit/
RUN npm install

# The run the start script
CMD [ "/bin/bash" ]
