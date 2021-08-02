#include <cmath>
#include <sstream>

#include "absl/strings/str_format.h"
#include "base/status_macros.h"
#include "eval/public/activation.h"
#include "eval/public/builtin_func_registrar.h"
#include "eval/public/cel_expr_builder_factory.h"
#include "eval/public/cel_expression.h"
#include "eval/public/cel_value.h"
#include "gmock/gmock.h"
#include "google/protobuf/text_format.h"
#include "gtest/gtest.h"

namespace google {
namespace api {
namespace expr {
namespace runtime {

namespace {

// glog syntax is LOG(level)
#define LOG(level) GOOGLE_LOG(level)

// Helper function for testing integer overflow
template <typename T>
void TestIntOverflow(const T const_expr_value, const uint64_t var,
                     const bool expect_parsing_success = true) {
  static constexpr absl::string_view kIntOperatorExpr =
      R"pb(
           id: 2
           call_expr {
             function: "_*_"
             args {
               id: 1
               ident_expr { name: "var" }
             }
             args {
               id: 3
               const_expr { int64_value: %s }
             }
           }
      )pb";

  v1alpha1::Expr expr;
  v1alpha1::SourceInfo source_info;
  // Make the const_expr int64_value a large int
  const bool parsing_success = protobuf::TextFormat::ParseFromString(
      absl::StrFormat(kIntOperatorExpr, const_expr_value), &expr);
  ASSERT_EQ(parsing_success, expect_parsing_success);

  if (parsing_success) {
    std::unique_ptr<CelExpressionBuilder> builder =
        CreateCelExpressionBuilder();
    ASSERT_OK(RegisterBuiltinFunctions(builder->GetRegistry()));

    auto cel_expression_status = builder->CreateExpression(&expr, &source_info);
    ASSERT_OK(cel_expression_status);
    auto cel_expression = std::move(cel_expression_status.value());

    Activation activation;

    activation.InsertValue("var", CelValue::CreateInt64(var));

    protobuf::Arena arena;
    auto eval_status = cel_expression->Evaluate(activation, &arena);
    EXPECT_TRUE(!eval_status.ok() ||
                eval_status.value().type() == CelValue::Type::kError);
    if (eval_status.ok()) {
      LOG(INFO) << "Eval result: " << eval_status.value().DebugString();
    } else {
      LOG(ERROR) << "eval status message: "
                 << eval_status.status().message().data();
    }
  }
}

// Tests int64 values or results of int operations that are numbers greater
// than or equal to 2^63
TEST(FuzzingTest, IntOverflow) {
  const int64_t kInt64MaxValue = std::pow(2.0, 63.0) - 1;
  const int kMaxValueDigits = std::log10(kInt64MaxValue) + 1;

  // The resulting product of the two args will have an int overflow
  TestIntOverflow(std::pow(2.0, 62.0), std::pow(2.0, 62.0));
  // Make var larger than max int64 size
  TestIntOverflow(5, static_cast<uint64_t>(kInt64MaxValue) + 1u);

  // Make the const_expr int very large
  std::stringstream big_int;
  for (int i = 0; i < kMaxValueDigits * 2; i++) {
    big_int << '1';
    if (i > kMaxValueDigits) {
      TestIntOverflow(big_int.str(), 30, false);
    }
  }
}

}  // namespace
}  // namespace runtime
}  // namespace expr
}  // namespace api
}  // namespace google
