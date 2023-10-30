package past.LoadBalancer
import org.http4s.*

// System design - implement of mini Twitter
// Scaling - Load balancer
// hot do you scetch balancer

opaque type Urls = Vector[Uri]


object LoadBalancer {
  def build (
    //TODO 
    backends: Urls,
    sendAndExpect: (Request[IO], Uri) => IO[String],
    updateFunction: Urls => Urls,
    extractor: Urls => Option[Uri]
  ): HttpRoutes[IO] = {
    ???
  }
}
