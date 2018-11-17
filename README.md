# REST service in pure FP

Demonstrates how to use Haskell and PostgreSql to implement a microservice running inside a docker container.  It provides the functionality of access a database.

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
For the requirement in compiling the code using PostgreSql-simple, it has to install `libpq-dev`.  It is because `pg_config` program is required, http://www.lambda-land.com/posts/2017-11-16-postgresql-simple

### Install PostgreSql db docker container
`docker pull postgres:latest`

`docker run --name type-safe-ws-db -p 5445:5432 -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=password -d postgres`

### Build and start the application
`stack build`

`stack exec type-safe-ws-exe`

### For unit-test
`stack build type-safe-ws\:test\:type-safe-ws-test`

## Deploy application to Docker container

### Build Docker image
`docker build -t jinilover/type-safe-ws:latest .`

### Run Docker image
```
docker run -dit --name type-safe-ws --network host \
-e DB_HOST=localhost \
-e DB_NAME=postgres \
-e DB_USER=postgres \
-e DB_PASSWORD=password \
-e DB_PORT=5445 \
-e RESRC_LOC=/opt/resources \
jinilover/type-safe-ws:latest
```
### Publish to docker hub
`docker login --username=jinilover`

`docker push jinilover/type-safe-ws:latest`

## References
* https://futtetennismo.me/posts/docker/2017-11-24-docker-haskell-executables.html
* https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
