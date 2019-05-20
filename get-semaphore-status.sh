#!/bin/bash

while sleep 30; do
  echo "git semaphore --status | jq [.result] > .test-result"
done
