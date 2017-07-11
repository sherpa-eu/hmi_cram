cmake_minimum_required(VERSION 2.8.3)
project(hmi_cram)
find_package(catkin REQUIRED COMPONENTS roslisp)
catkin_package(
CATKIN_DEPENDS
)
include_directories(
  ${catkin_INCLUDE_DIRS}
)

add_lisp_executable(hmi-main
hmi-cram hmi-cram::hmi-main)

add_lisp_executable(proactive-main
hmi-cram hmi-cram::proactive-main)