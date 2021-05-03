package barneshut

import conctrees.ConcBuffer

trait SectorMatrixInterface {
  def +=(b: Body): SectorMatrix
  def combine(that: SectorMatrix): SectorMatrix
  def apply(x: Int, y: Int): ConcBuffer[Body]
}

trait QuadInterface {
  def massX: Float
  def massY: Float
  def mass: Float
  def centerX: Float
  def centerY: Float
  def size: Float
  def total: Int
  def insert(b: Body): Quad
}
