kaist_palettes <- list(
  TestColor = c("#F18E10", "#F22606", "#780080")
)

kaist_palette <- function(name, n, type = c("discrete", "continuous")) {
  # type 패러미터를 가져옵니다.
  type <- match.arg(type)

  # wes_palettes 리스트에서 주어진 이름을 가지는 색 목록을 가져옵니다.
  pal <- kaist_palettes[[name]]
  # 해당되는 이름의 색 목록이 없다면 에러를 표시합니다.
  if (is.null(pal))
    stop("Palette not found.")

  # n이 주어지지 않았다면 가져온 색 목록의 색 개수로 n을 정합니다.
  if (missing(n)) {
    n <- length(pal)
  }

  # 만약 type이 "discrete"이고 n이 색 목록의 색 개수보다 많다면 에러를 표시합니다.
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  # 만약 type이 continuous라면 grDevices의 colorRampPalette 함수를 이용해서
  # n개만큼의 색을 가지는 리스트를 만듭니다.
  # colorRampPalette는 주어진 색의 중간 색을 interpolation해서 원하는 수만큼 만들어주는 함수입니다.
  #
  # discrete이라면 가져온 색 목록에서 n개만큼의 색을 반환합니다.
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  # structure 함수는 out 변수에 class와 name attribute를 추가해주는 역할을 합니다.
  structure(out, class = "palette", name = name)
}
