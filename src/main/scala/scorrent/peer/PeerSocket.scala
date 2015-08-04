package scorrent.peer

import scala.concurrent.Future
import java.net.{ServerSocket, Socket}
import concurrent.ExecutionContext.Implicits.global

object PeerSocket {
  def acceptPeers(port: Int, onPeerConnect: (Socket) => Unit) {
    new Thread(new Runnable {
      def run() {
        while(true) {
          val serverSocket = new ServerSocket(port)
          val socket = serverSocket.accept
          val f = Future { onPeerConnect(socket) }
          f onComplete { case _ => socket.close }
        }
      }
    } ).start
  }
}
