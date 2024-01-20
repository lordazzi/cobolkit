# Define a imagem base que será usada
FROM nginx

# Copie o arquivo de configuração personalizado para dentro do container
COPY nginx.conf /etc/nginx/nginx.conf
COPY html /usr/share/nginx/html

# Exponha a porta 80 para o host
EXPOSE 80

# Comando para iniciar o servidor Nginx
CMD ["nginx", "-g", "daemon off;"]