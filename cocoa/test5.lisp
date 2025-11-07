#import <Cocoa/Cocoa.h>
#include <dlfcn.h>
#include <stdio.h>
#include <ffi.h>

int test_cif(cif *ffi_cif) {
  void * library = dlopen("libobjc.dylib",0);
  void * myobjc_msgSend = (void*)dlsym(library, "objc_msgSend");
  SEL sel = @selector(test:);
  ffi_type *args[3];
  void* values[3];
  ffi_type ms_type;
  ffi_type *ms_type_elements[4];
  ms_type.size = 0;
  ms_type.alignment = 0;
  ms_type.type = FFI_TYPE_STRUCT;
  ms_type.elements = ms_type_elements;
  ms_type_elements[0] = &ffi_type_double;
  ms_type_elements[1] = &ffi_type_double;
  ms_type_elements[2] = &ffi_type_double;
  ms_type_elements[3] = NULL;
  args[0] = &ffi_type_pointer;
  args[1] = &ffi_type_pointer;
  args[2] = &ms_type;
  if (ffi_prep_cif(cif, FFI_DEFAULT_ABI, 3, &ms_type, args) == FFI_OK) {
    return 1;
  } else {
    return 0;
  }
}

//clang -lffi -dynamiclib -framework Foundation -o test4 -I/Library/Developer/CommandLineTools/SDKs/MacOSX15.sdk/usr/include/ffi test5.m
