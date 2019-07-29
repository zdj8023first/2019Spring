#!/usr/bin/env bash
# pull镜像
docker pull mongo

# 关闭Selinux 如果无法启动的话
# setenforce 0

#进入数据目录
cd ~/mongo

# 启动镜像
docker run -idt --name=mongo_bj -p 27017:27017 -v $PWD/db_bj:/data/db2  mongo --bind_ip_all

# 运行