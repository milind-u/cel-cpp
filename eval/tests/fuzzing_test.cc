#include <algorithm>
#include <cmath>
#include <memory>
#include <sstream>
#include <string>

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

std::unique_ptr<CelExpression> CreateExpression(
    const v1alpha1::Expr &expr, const bool expect_expr_ok = true,
    const bool check_ok = true) {
  auto builder = CreateCelExpressionBuilder();
  EXPECT_OK(RegisterBuiltinFunctions(builder->GetRegistry()));
  v1alpha1::SourceInfo source_info;

  auto cel_expr_status = builder->CreateExpression(&expr, &source_info);
  if (check_ok) {
    EXPECT_EQ(cel_expr_status.ok(), expect_expr_ok);
  }
  if (!cel_expr_status.ok()) {
    LOG(ERROR) << cel_expr_status.status().message().data();
    return nullptr;
  }
  return std::move(cel_expr_status.value());
}

absl::StatusOr<CelValue> Eval(std::unique_ptr<CelExpression> cel_expr,
                              const Activation &activation,
                              const bool expect_err = true) {
  EXPECT_TRUE(cel_expr);

  protobuf::Arena arena;
  const auto eval_status = cel_expr->Evaluate(activation, &arena);

  if (expect_err) {
    EXPECT_TRUE(!eval_status.ok() ||
                eval_status.value().type() == CelValue::Type::kError);
    if (eval_status.ok()) {
      LOG(INFO) << "Eval result: " << eval_status.value().DebugString();
    } else {
      LOG(ERROR) << "eval status message: "
                 << eval_status.status().message().data();
    }
  }
  return eval_status;
}

// Helper function for testing integer overflow
template <typename T>
void TestIntOverflow(const T &const_expr_value, const uint64_t var,
                     const bool expect_parsing_success = true) {
  static constexpr absl::string_view kIntOperatorExpr = R"pb(
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
  // Make the const_expr int64_value a large int
  const bool parsing_success = protobuf::TextFormat::ParseFromString(
      absl::StrFormat(kIntOperatorExpr, const_expr_value), &expr);
  ASSERT_EQ(parsing_success, expect_parsing_success);

  if (parsing_success) {
    auto cel_expr = CreateExpression(expr);
    Activation activation;
    activation.InsertValue("var", CelValue::CreateInt64(var));
    Eval(std::move(cel_expr), activation).IgnoreError();
  }
}

// Tests int64 values or results of int operations that are numbers greater
// than or equal to 2^63
TEST(FuzzingTest, IntOverflow) {
  const int64_t kInt64MaxValue = std::pow(2.0, 63.0) - 1.0;
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

void TestNoArgsToOperator(const std::string &ast) {
  LOG(INFO) << "ast: " << ast;

  v1alpha1::Expr expr;
  ASSERT_TRUE(protobuf::TextFormat::ParseFromString(ast, &expr));

  auto cel_expr = CreateExpression(expr, false, false);
  if (cel_expr) {
    Activation activation;
    Eval(std::move(cel_expr), activation).IgnoreError();
  }
}

// Tests expressions containing operators without arguments and with blank ones
TEST(FuzzingTest, NoArgsToOperator) {
  constexpr absl::string_view kNoArgsExpr = R"(
    call_expr: <
      function: "%s"
    >
  )";
  constexpr absl::string_view kBlankArgsExpr = R"(
    call_expr: <
      function: "%s"
      args: <
      >
      %s
    >
  )";
  constexpr absl::string_view kEmptyArgs = "args: <>";

  constexpr std::array<absl::string_view, 17> kOperators = {
      "_!",  "-_",  "_*_",  "_/_",  "_%_",  "_+_",  "_-_",  "_==_", "_!=_",
      "_<_", "_>_", "_<=_", "_>=_", "_in_", "_&&_", "_||_", "_?_:_"};
  for (const auto operator_symbol : kOperators) {
    TestNoArgsToOperator(absl::StrFormat(kNoArgsExpr, operator_symbol));

    const int num_operands =
        std::count(operator_symbol.begin(), operator_symbol.end(), '_');

    // First empty arg is already included
    std::stringstream empty_args;
    for (int i = 0; i < num_operands - 1; i++) {
      empty_args << kEmptyArgs;
      if (i != num_operands - 2) {
        empty_args << "\n      ";
      }
    }

    const auto ast =
        absl::StrFormat(kBlankArgsExpr, operator_symbol, empty_args.str());
    TestNoArgsToOperator(ast);
  }
}

void FlipBitAt(const int bit, std::string &str) {
  const int byte = bit / 8;
  const int bitInByte = bit % 8;
  str[byte] ^= (1 << bitInByte);
}

void FlipByteAt(const int byte, std::string &str) {
  for (int i = 0; i < 8; i++) {
    FlipBitAt((byte * 8) + i, str);
  }
}

void TestFlippingBits(const std::string &ser_expr,
                      const v1alpha1::SourceInfo &source_info,
                      CelExpressionBuilder *builder) {
  v1alpha1::Expr expr;
  expr.ParseFromString(ser_expr);

  auto cel_expr_status = builder->CreateExpression(&expr, &source_info);

  if (cel_expr_status.ok()) {
    auto cel_expr = std::move(cel_expr_status.value());

    Activation activation;
    activation.InsertValue("var", CelValue::CreateInt64(12));

    Eval(std::move(cel_expr), activation, false);
  }
}

// Tests trying to compile and evaluate an expression after
// flipping bits in its serialized format so that
// it continues reading an int value past its actual end.
TEST(FuzzingTest, FlippingBitsInSerializedExpr) {
  constexpr auto kExpr = R"(
    call_expr: <
      function: "_+_"
      args: <
        ident_expr: <
          name: "var"
        >
      >
      args: <
        const_expr: <
          int64_value: 1
        >
      >
    >
  )";

  v1alpha1::Expr expr;
  ASSERT_TRUE(protobuf::TextFormat::ParseFromString(kExpr, &expr));

  std::string ser_expr;
  ASSERT_TRUE(expr.SerializeToString(&ser_expr));

  auto builder = CreateCelExpressionBuilder();
  EXPECT_OK(RegisterBuiltinFunctions(builder->GetRegistry()));
  v1alpha1::SourceInfo source_info;

  // Try flipping each bit in the serialized expression
  for (size_t i = 0; i < ser_expr.size() * 8; i++) {
    FlipBitAt(i, ser_expr);
    TestFlippingBits(ser_expr, source_info, builder.get());
    FlipBitAt(i, ser_expr);
  }

  // Try flipping each byte in the serialized expression
  for (size_t i = 0; i < ser_expr.size() - 1; i++) {
    FlipByteAt(i, ser_expr);
    TestFlippingBits(ser_expr, source_info, builder.get());
    FlipByteAt(i, ser_expr);
  }

  // Flip random bits
  constexpr int kRandSeed = 34531;
  std::srand(kRandSeed);
  for (int i = 0; i < 1000; i++) {
    const std::string ser_expr_cp = ser_expr;
    const int bits_to_flip = std::rand() % (ser_expr_cp.size() / 3);
    for (int j = 0; j < bits_to_flip; j++) {
      FlipBitAt(std::rand() % ser_expr_cp.size(), ser_expr);
      TestFlippingBits(ser_expr_cp, source_info, builder.get());
    }
  }
}

}  // namespace
}  // namespace runtime
}  // namespace expr
}  // namespace api
}  // namespace google
