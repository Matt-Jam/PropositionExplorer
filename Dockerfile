FROM haskell:latest

WORKDIR /app 

COPY . .

CMD ["bash"]