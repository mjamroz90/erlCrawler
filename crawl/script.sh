#!/bin/bash

cat crawler.sql | grep http | awk -F\' '{print $2}' > urls
