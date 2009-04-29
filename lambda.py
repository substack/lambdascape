#!/usr/bin/python2.5
# based on http://opende.sourceforge.net/wiki/index.php/HOWTO_4_wheel_vehicle
# and http://pyode.sourceforge.net/tutorials/tutorial3.html

import ode
import threading, time, re

class Physics :
    def __init__(self) :
        # Create a world object
        self.world = ode.World()
        #self.world.setGravity( (0,-9.81,0) )
        self.world.setGravity( (0,-0.1,0) )
        self.world.setERP(0.8)
        self.world.setCFM(1E-5)
         
        self.space = ode.Space()
        self.bodies = {}
        
        self.floor = ode.GeomPlane(self.space, (0,1,0), 0)
        
        self.fps = 25.0
        self.dt = 1.0 / self.fps
        
        self.last_update = time.time()
        self.done = False
        self.terrain = None
    
    def load_terrain(self, file) :
        # build terrain triangles from an image
        import gd
        im = gd.image(file)
        width, height = im.size()
        
        coords = [ # terrain spread from -5.0 to +5.0
            (
                x - width / 2,
                y - height / 2,
                im.red(im.getPixel((x,y))) / 255.0 # 0 to 1 from red channel
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
            if x >= width / 2 - 1 or y >= height / 2 - 1 : continue
            # 1 - 2
            # | / |  <-- how triangles are built out of quads
            # 3 - 4
            p1 = (x, y)
            p2 = (x + 1, y)
            p3 = (x, y + 1)
            p4 = (x + 1, y + 1)
            # triangles are groups of three indices
            faces.append((grid[p1], grid[p2], grid[p3]))
            faces.append((grid[p2], grid[p3], grid[p4]))
        
        self.terrain = {
            "verts" : verts,
            "faces" : faces,
        }
        mesh = ode.TriMeshData()
        mesh.build(verts, faces)
        geom = ode.GeomTriMesh(mesh, self.space)
        geom.enable()
        geom.setPosition((0.0,0.0,0.0))
    
    def robot(self, name, x, y, z) :
        box = ode.Body(self.world)
        mass = ode.Mass()
        mass.setBox(1000, 1.0, 1.0, 1.0)
        box.setMass(mass)
        box.shape = "box"
        box.boxsize = (1.0, 1.0, 1.0)
        
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
    
    # Collision callback
    def _near_callback(self, args, geom1, geom2) :
        contacts = ode.collide(geom1, geom2)
        # Create contact joints
        world,contactgroup = args
        for c in contacts:
            c.setBounce(0.2)
            c.setMu(5000)
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
                if line == "" : break
                if line.strip() == "quit" : break
                if line.strip() == "shutdown" :
                    self.done = True
                    break
                if line.strip() == "terrain" :
                    sock.sendall("%(verts)s\n%(faces)s\n" % self.terrain)
            
            if self.last_update > last :
                # time to push an update to the observer
                last = time.time()
                # send all robot positions and rotations
                for (name, body) in self.bodies.iteritems() :
                    pos = body.getPosition()
                    rot = body.getRotation()
                    sock.sendall('("%s",%s,%s)\n' % (name, pos, list(rot)))
    
    def _handle_robot(self, sock, client, name) :
        body = self.bodies[name]
        while not self.done :
            line = self.getline(sock)
            if line is None :
                time.sleep(0.0)
                continue
            elif line == "" : break
            
            cmd = line.split()[0]
            if cmd == "quit" : break
            elif cmd == "position" :
                pos = body.getPosition()
                rot = body.getRotation()
                sock.sendall('(%s,%s)\n' % (pos, list(rot)))
            elif cmd == "distance" :
                # TODO: compute the actual distance
                sock.sendall("0.0\n")
            elif cmd == "motor" :
                # TODO: motors
                pass
            
    def listen(self) :
        print "Spawning service on port %s" % self.port
        
        import socket
        server = socket.socket()
        server.bind(("localhost", int(opts["port"])))
        server.listen(5)
        try :
            while not self.done :
                sock, client = server.accept() # we get signal
                sock.setblocking(1) # blocking socket
                threading.Thread(
                    None, self._handle_client, "handler", [ sock, client ]
                ).start()
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
        contactgroup = ode.JointGroup()
        self.space.collide((self.world, contactgroup), self._near_callback)
        
        self.world.step(self.dt)
        
        # Remove all contact joints
        contactgroup.empty()
        
        self.last_update = time.time()

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
        sys.argv[1:], "", "port= map=".split(),
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
    
    physics.run()
