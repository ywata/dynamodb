#!/bin/bash

curl=`which curl`
jar=`which jar`

#ddb=dynamodb_local_2013-09-12
#ddbtargz=${ddb}.tar.gz
#ddblocal="https://s3-us-west-2.amazonaws.com/dynamodb-local/${ddbtargz}"
#dynamojar=DynamoDBLocal.jar
source dynamo.conf

if [ ! -x ${curl} ] ; then
    echo "curl not found on this system."
    exit 1
fi

if [ ! -e ${ddbtargz} ] ; then
    ${curl} --get ${ddblocal} > ${ddbtargz}
fi

tar xf ${ddbtargz}


if [ ! -x ${jar} ] ; then
    echo "${jar} not found"
    exti 1
fi

if [ ! -d ${ddb} ] ; then
    echo "${ddb} not found."
    exit 1
fi
cd ${ddb}

if [ ! -f ${dynamojar} ] ; then
    echo "${dynamojar} not found"
fi
${jar} uf ${dynamojar} ../log4j.xml 
${jar} uf ${dynamojar} ../log4j.properties

