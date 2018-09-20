# REST service in pure FP

A poc that demonstrates how to use Haskell and PostgreSql to implement a microservice.  It provides the functionality of access a database.

## Tools
* Haskell-servant
* PostgreSql-simple
* PostgreSql-simple-migration, similar to FlywayDb
* Configurator
* Docker container for PostgresSql
* Stack

## API endpoints

### List all users
```
GET /users
``` 
Example
```
curl http://localhost:9001/
```

### List all users in sorting order
```
GET /users?sortBy=name
```
Example
```
curl http://localhost:9001/users?sortBy=name
```

### Add user
```
POST /users
```
Example
```
curl -i --header "Content-Type: application/json"   \
--request POST   \
--data '{"email":"isaac@newton.co.uk","registrationDate":"1683-03-1","age":372,"name":"Isaac Newton"}' \
http://localhost:9001/users
```

### Remove user
```
DELETE /users/user_name
```
Example
```
curl -i -X DELETE http://localhost:9001/users/Isaac%20Newton
```

## Setup environment

### Install PostgreSql
For the requirement in compiling the code using PostgreSql-simple, 
`brew install postgresql` for mac

### Install PostgreSql db docker container
`docker pull postgres:latest`
`docker run --name type-safe-ws-db -p 5445:5432 -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=password -d postgres`

### Build and start the application
`stack build`
`stack exec type-safe-ws-exe`

### For unit-test
`stack build type-safe-ws\:test\:type-safe-ws-test`

## TODO
Add an API to display the GIT information.  This is very useful in providing the version of the source being deployed in different environments.


