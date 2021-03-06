#!/usr/bin/python2.5

import ode
import threading, time, re

class Physics :
    def __init__(self) :
        # Create a world object
        self.world = ode.World()
        self.world.setGravity((0,0,-3.0))
        
        self.world.setERP(0.1) # error correction each time step
        self.world.setCFM(0.0) # rigid bodies
         
        self.space = ode.Space()
        self.bodies = {}
        
        self.fps = 50.0
        self.dt = 1.0 / self.fps
        
        self.last_update = time.time()
        self.done = False
        self.terrain = None
        self.terrain_geom = None
    
    def load_terrain(self, file) :
        "build terrain triangles from an image that gd can read"
        import gd
        im = gd.image(file)
        width, height = im.size()
        # scale the terrain
        sx, sy, sz = 4, 4, 4
        
        coords = [ # terrain spread from -5.0 to +5.0
            (
                (x - width / 2) * sx,
                (y - height / 2) * sy,
                im.red(im.getPixel((x,y))) / 255.0 * sz
            )
            for x in range(width) for y in range(height)
        ]
        grid = {}
        
        verts = []
        for (x,y,z) in coords :
            grid[(x,y)] = len(verts) # index into verts
            verts.append((x,y,z))
         
        faces = []
        for (x,y,z) in coords :
            if x >= sx * (width / 2 - 1) or y >= sy * (height / 2 - 1) :
                continue
            # 1 - 2
            # | / |  <-- how triangles are built out of quads
            # 3 - 4
            p1 = (x, y)
            p2 = (x + sx, y)
            p3 = (x, y + sy)
            p4 = (x + sx, y + sy)
            # triangles are groups of three indices
            faces.append((grid[p1], grid[p2], grid[p3]))
            faces.append((grid[p2], grid[p3], grid[p4]))
        
        self.terrain = {
            "verts" : verts,
            "faces" : faces,
        }
        mesh = ode.TriMeshData()
        mesh.build(verts, faces)
        self.terrain_geom = ode.GeomTriMesh(mesh, self.space)
        self.terrain_geom.setPosition((0.0,0.0,0.0))
    
    def robot(self, name, x, y, z) :
        """
            Create a new robot with a name positioned at (x,y,z)
        """
        box = ode.Body(self.world)
        mass = ode.Mass()
        mass.setBox(1000, 1.0, 1.0, 1.0)
        box.setMass(mass)
        box.shape = "box"
        box.boxsize = (10.0, 10.0, 10.0)
        
        geom = ode.GeomBox(self.space, lengths=box.boxsize)
        geom.setBody(box)
        box.setPosition(tuple(float(i) for i in [x, y, z]))
        
        import math
        theta = 0
        ct = math.cos (theta)
        st = math.sin (theta)
        box.setRotation([
            ct, 0.0, -st,
            0.0, 1.0, 0.0,
            st, 0.0, ct
        ])
        self.bodies[name] = box
        
    def lidar(self, name, vx, vy, vz) :
        """
            With a LIDAR-type instrument, query the distance to a solid object
            in a direction
        """
        body = self.bodies[name]
        # create a ray facing the specified direction
        ray = ode.GeomRay(self.space, rlen=100.0)
        ray.set(body.getPosition(), (vx, vy, vz))
        
        contacts = ode.collide(self.terrain_geom, ray)
        for geom in self.bodies.itervalues() :
            if geom != body :
                contacts.extend( ode.collide(geom, ray) )
        
        if contacts == [] : return -1.0 # no collisions
        distances = []
        px, py, pz = body.getPosition()
        for contact in contacts :
            pos, normal, depth, geom1, geom2 = contact.getContactGeomParams()
            x, y, z = pos
            distances.append((
                (x - px) ** 2 + (y - py) ** 2 + (z - pz) ** 2
            ))
        return min(distances) # returns closest collision
        
    # Collision callback
    def _near_callback(self, args, geom1, geom2) :
        contacts = ode.collide(geom1, geom2)
        # Create contact joints
        world,contactgroup = args
        for c in contacts:
            c.setBounce(0.02)
            c.setMu(1000.0)
            j = ode.ContactJoint(world, contactgroup, c)
            j.attach(geom1.getBody(), geom2.getBody())
    
    def _handle_client(self, sock, client) :
        name = None
        try :
            line = self.getline(sock, None)
            if line is None :
                raise "Something blew up"
            elif line.strip() == "observer" :
                self._handle_observer(sock, client)
            else :
                name, x, y, z = line.split()
                if name in self.bodies :
                    name = None # don't want to pop off for naming conflicts >_<
                    raise Exception("A robot by that name already exists")
                self.robot(name, x, y, z)
                self._handle_robot(sock, client, name)
            
        except :
            import traceback
            sock.sendall(traceback.format_exc())
        finally :
            sock.close()
            if name in self.bodies : self.bodies.pop(name)
    
    def _handle_observer(self, sock, client) :
        last = 0.0
        while not self.done :
            line = self.getline(sock)
            if not line is None :
                cmd = line.strip()
                if cmd == "" : break
                elif cmd == "quit" : break
                elif cmd == "shutdown" :
                    self.done = True
                    break
                if line.strip() == "terrain" :
                    sock.sendall("%(verts)s\n%(faces)s\n" % self.terrain)
            
            if self.last_update > last :
                # time to push an update to the observer
                last = time.time()
                # send all robot positions and rotations
                for (name, body) in self.bodies.iteritems() :
                    rot = list(body.getRotation()) # 9-tuple as 3x3 matrix
                    x,y,z = body.getPosition()
                    # set the translation and convert to a row-major 4x4
                    mat = (
                        rot[0:3] + [x] +
                        rot[3:6] + [y] +
                        rot[6:9] + [z] +
                        [0.0, 0.0, 0.0, 1.0]
                    )
                    sock.sendall('("%s",%s)\n' % (name, mat))
    
    def _handle_robot(self, sock, client, name) :
        body = self.bodies[name]
        while not self.done :
            line = self.getline(sock)
            if line is None :
                time.sleep(0.0)
                continue
            elif line == "" : break
            
            cmd = line.split()[0]
            args = line.split()[1:]
            if cmd == "quit" : break
            elif cmd == "shutdown" :
                self.done = True
                break
            elif cmd == "position" :
                pos = body.getPosition()
                rot = body.getRotation()
                sock.sendall('(%s,%s)\n' % (pos, list(rot)))
            elif cmd == "lidar" :
                x, y, z = map(float, args)
                sock.sendall("%f\n" % self.lidar(name, x, y, z))
            elif cmd == "force" :
                # no wheels for now, rocket power
                self.bodies[name].addRelForce(tuple(
                    float(x) for x in args
                ))
            
    def listen(self) :
        print "Spawning service on port %s" % self.port
        
        import socket
        server = socket.socket()
        try :
            server.bind(("localhost", int(self.port)))
            server.listen(5)
            while not self.done :
                sock, client = server.accept() # we get signal
                sock.setblocking(1) # blocking socket
                threading.Thread(
                    None, self._handle_client, "handler", [ sock, client ]
                ).start()
        except : # probably address is already in use
            self.port += 1 # next port
            self.listen()
        finally :
            self.done = True
            server.close()
    
    def run(self) :
        threading.Thread(
            None, physics.listen, "listener", []
        ).start()
        
        self.done = False
        try :
            while not self.done :
                self.step()
        finally :
            self.done = True
            time.sleep(0.4)
            sys.exit(0)
    
    def step(self) :
        elapsed = time.time()
        
        contactgroup = ode.JointGroup()
        self.space.collide((self.world, contactgroup), self._near_callback)
        
        self.world.step(self.dt)
        
        # Remove all contact joints
        contactgroup.empty()
        
        self.last_update = time.time()
        elapsed = time.time() - elapsed
        if elapsed < self.dt : # simulation is too fast
            time.sleep(self.dt - elapsed)

    def getline(self, sock, t=0.01) :
        import select
        rh, wh, eh = select.select([sock], [], [], t)
        if rh == [] : return None
        
        line = ""
        while not self.done :
            c = sock.recv(1)
            line += c
            if c == "\n" : break
        return line
    

if __name__ == "__main__" :
    physics = Physics()
    
    import getopt, sys
    optlist, cmd = getopt.getopt(
        sys.argv[1:], "", "port= map= fps= ".split(),
    )
    opts = dict(
        (re.sub(r"^--", "", k),v)
        for (k,v) in optlist
    )
    
    if not "port" in opts :
        opts["port"] = 9001 # OVER NINE THOUSAND
    physics.port = int(opts["port"])
    
    if not "map" in opts :
        opts["map"] = "map.png"
    physics.load_terrain(opts["map"])
    
    if not "fps" in opts :
        opts["fps"] = 25.0
    physics.fps = float(opts["fps"])
    physics.dt = 1.0 / physics.fps
    
    physics.run()
