package past.LoadBalancer
import org.http4s.*
import cats.syntax.all.*
import cats.effect.kernel.ref
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import org.http4s.dsl.Http4sDsl
import com.comcast.ip4s.Port
import com.comcast.ip4s.Host
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.client.Client
import org.http4s.client.middleware.Logger

                                                        // System design - implement of mini Twitter
                                                        // Scaling - Load balancer
                                                        // hot do you scetch balancer

opaque type Urls = Vector[Uri]
object Urls {
  def apply(urls: Vector[Url]): Urls = urls

  val  roundRobin: Urls => Urls = vector => 
    if (vector.isEmpty) vector
    else vector.tail + vector.head

  val first: Urls => Option[Uri] = 
    _.headOption
}

object LoadBalancer {
  def apply (
    //TODO 
    backends: Ref[IO, Urls],
    sendAndExpect: (Request[IO], Uri) => IO[String],    // Redirecting HTTP requests.
    addPathToBackend: (Request[IO],Uri) => IO[Uri],    // Massage te HTTP request for teh replica. 
    updateFunction: Urls => Urls,                       // Shuffle the list of backends.
    extractor: Urls => Option[Uri]                      // Extract the backend for which I will send my new HTTP  requests. 
  ): Resource[IOHttpRoutes[IO]] = {
    val dsl = Http4sDsl[IO]
    import dsl.* 

    val routes = HttpRoutes.of[IO] { request => 
      // Extract the replica.
      // Forward request.
      // Shuffle the replica.
      // Return response to the user.
      backends.getAndUpdate(updateFunction).map(extractor).flatMap {
        _.fold(Ok("All backend are inactive.")) { backendUri => 
          for {
            uri <- addPathToBackend(request,backendUri)
            response <- sendAndExpect(request, uri)
            result <- Ok(response)
          } yield result

        }
      } // IO[Option[Uri]]      
    }
    Resource.pure(routes)
  }
}

// Actual Application. 

object Replica extends IOApp {
  override def run (args: List[String]): IO[ExitCode] = {
  val port = args(0).toInt
  val host = "localhost"

  val dsl = Http4sDsl[IO]
  import dsl.*
  val routes = HttpRoutes.of[IO] { request => 
    OK(s"[replica:$port] You are accessed: ${request.uri.path}")

  }

  // val portOption = Port.fromInt(port)

  val Server = for {
    h <- Host.fromString(host)
    p <- Port.fromInt(port)

  } yield  ServerBuilder
    .default[IO]
    .withHost(h)
    .withPort(p)
    .withHttpApp(routes.orNotFound)
    .build

  Server.map(_.use(_ => IO.prinln(s"Replica - port $port") *> IO.never))
    .getOrElse(IO.println("Host/Port are not working. "))
    .as(ExitCode.success)

 }
}


object BigApp extends IOApp.Simple {

  def sendReq(client: Client[IO]) = (req: Request[IO], uri:Uri) => 
    client.expect[String](req.withUri(uri))

  def addReqPathToBackend(req: Request[IO], uri: Uri): IO[Uri] = 
    IO.pure {
      uri / req.uri.path.renderString.dropWhile(_ != '/')  // /user/usename?id=111
    }

  def getSeedNodes: IO[Urls] = IO.fromEither(
    ConfigSource.default.at("backends").load[List[String]]
      .map(strings => strings.map(Uri.unsafeFromString))  // IO[List[Uri]]
      .map(_.toVector)
      .map(_.toUrls)  // IO[Urls]
      .leftMap(e => new java.lang.RuntimeException(" Cannot parse " + e))
  )
  override def run: IO[Unit] = {
    val serverResource = for {
      seedNodes <- Resource.eval(getSeedNodes)
      backends <- Resource.eval(Ref.of[IO, Urls](seedNodes))
      client <- EmberServerBuilder.default[IO].build
      loadBalancer <- LoadBalancer(
        backends,
        sendReq(client),
        addReqPathToBackend,
        Urls.roundRobin,
        Urls.first

      )
      server <- EmberServerBuilder
      .default[IO]
      .withHost(host"localhost")
      .withPort(port"8080")
      .withHttpApp(loadBalancer.orNotFound)
    } yield server

    serverResource.use(_ => IO.prinln(" load balancer") *> IO.never)
  }
}