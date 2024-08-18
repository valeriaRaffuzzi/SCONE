# CMake generated Testfile for 
# Source directory: /home/mskrette/SCONE_cambridge_fork/SCONE
# Build directory: /home/mskrette/SCONE_cambridge_fork/SCONE/Build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(Unit_Tests "/home/mskrette/SCONE_cambridge_fork/SCONE/Build/unitTests")
set_tests_properties(Unit_Tests PROPERTIES  _BACKTRACE_TRIPLES "/home/mskrette/SCONE_cambridge_fork/SCONE/CMakeLists.txt;251;add_test;/home/mskrette/SCONE_cambridge_fork/SCONE/CMakeLists.txt;0;")
add_test(Integration_Tests "/home/mskrette/SCONE_cambridge_fork/SCONE/Build/integrationTests")
set_tests_properties(Integration_Tests PROPERTIES  WORKING_DIRECTORY "/home/mskrette/SCONE_cambridge_fork/SCONE" _BACKTRACE_TRIPLES "/home/mskrette/SCONE_cambridge_fork/SCONE/CMakeLists.txt;252;add_test;/home/mskrette/SCONE_cambridge_fork/SCONE/CMakeLists.txt;0;")
subdirs("RandomNumbers")
subdirs("LinearAlgebra")
subdirs("SharedModules")
subdirs("Visualisation")
subdirs("ParticleObjects")
subdirs("NamedGrids")
subdirs("NuclearData")
subdirs("Geometry")
subdirs("Tallies")
subdirs("CollisionOperator")
subdirs("TransportOperator")
subdirs("UserInterface")
subdirs("PhysicsPackages")
subdirs("DataStructures")
