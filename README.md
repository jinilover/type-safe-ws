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
--data '{"email":"isaac@newton.co.uk","registrationDate":"1683-03-01","age":372,"name":"Isaac Newton"}' \
http://localhost:9001/users

curl -i --header "Content-Type: application/json"   \
--request POST   \
--data '{"email":"ae@mc2.org","registrationDate":"1905-12-01","age":136,"name":"Albert Einstein"}' \
http://localhost:9001/users

curl -i --header "Content-Type: application/json"   \
--request POST   \
--data '{"email":"kg@uv.edu","registrationDate":"1906-04-28","age":112,"name":"Kurt Goedel"}' \
http://localhost:9001/users

curl -i --header "Content-Type: application/json"   \
--request POST   \
--data '{"email":"se@uw.edu","registrationDate":"1913-09-30","age":105,"name":"Samuel Eilenberg"}' \
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

### Display git information
```
GET /service_info
```
Example
```
curl http://localhost:9001/service_info
```
Json response will be something like
```
{
  "gitCommitMessage": "add initConnPool",
  "gitCommitDate": "Tue Sep 25 01:14:51 2018 +1000",
  "gitHash": "1fd86f71ef1b795aa94ac58468f273d51fcb3ec0",
  "gitBranch": "master"
}
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
