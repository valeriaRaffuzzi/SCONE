#include <memory>
#include "dagmc_fortran.hpp"

std::shared_ptr<moab::DagMC> dagmc;

// creates a dagmc ptr to put the geometry in
void create_dagmc_ptr() {
    dagmc = std::shared_ptr<moab::DagMC>();
}

// the main load file functionality which takes as 
// argument the dagmc filename 
int load_file(const char* filename) {
  moab::ErrorCode rval;
  rval = dagmc->load_file(filename);
  return rval;
}

// build the obb tree
int init_obb() {
  moab::ErrorCode rval;
  rval = dagmc->init_OBBTree();
  return rval;
}
