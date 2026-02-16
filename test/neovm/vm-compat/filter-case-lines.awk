BEGIN {
  case_prefix = "__NEOVM_CASE__\t";
}

{
  pos = index($0, case_prefix);
  if (pos > 0) {
    print substr($0, pos + length(case_prefix));
  }
}
