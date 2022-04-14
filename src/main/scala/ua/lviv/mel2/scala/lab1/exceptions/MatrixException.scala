package ua.lviv.mel2.scala.lab1.exceptions

class MatrixException(message: String) extends RuntimeException(message)

object DegenerateMatrixException extends MatrixException("Matrix is degenerate (det=0)")

object NotSquareMatrixException extends MatrixException("Matrix is not square")

object IncompatibleMatricesShapes extends MatrixException("Matrices have incompatible dimensions")

object NotEqualRowLengthException extends MatrixException("Not equal row length in matrix")

object BadMatrixShapeException extends MatrixException("BadMatrixShapeException")
