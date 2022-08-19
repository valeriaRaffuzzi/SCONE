#include "DagMC.hpp"

/* calls the dagmc constructor */
#ifdef __cplusplus
  extern "C" {
#endif
void create_dagmc_ptr();
#ifdef __cplusplus
  }
#endif

/* loads a dagmc file */
#ifdef __cplusplus
  extern "C" {
#endif
int load_file(const char* filename);
#ifdef __cplusplus
  }
#endif

/* build the obb tree */
#ifdef __cplusplus
  extern "C" {
#endif
int init_obb();
#ifdef __cplusplus
  }
#endif
