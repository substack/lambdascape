# based on http://opende.sourceforge.net/wiki/index.php/HOWTO_4_wheel_vehicle
# and http://pyode.sourceforge.net/tutorials/tutorial3.html

import ode

class Physics :
    def __init__(self) :
        # Create a world object
        self.world = ode.World()
        self.world.setGravity( (0,-9.81,0) )
        self.world.setERP(0.8)
        self.world.setCFM(1E-5)
    
        self.space = ode.Space()
        self.bodies = {}
        
        self.floor = ode.GeomPlane(space, (0,1,0), 0)
    
    def terrain(self, verts, faces) :
        # verts: [[float,float,float]]
        # faces: [[int,int,int]]
        data = ode.TriMeshData.build(verts, faces)
        ode.GeomTriMesh(data, self.space)
    
    def robot(self, name, x, y, z) :
        box = ode.Body(world)
        mass = ode.Mass()
        mass.setBox(1000, 1.0, 1.0, 1.0)
        body.setMass(mass)
        body.shape = "box"
        body.boxsize = (1.0, 1.0, 1.0)
        
        geom = ode.GeomBox(space, lengths=body.boxsize)
        geom.setBody(body)
        body.setPosition(0, 10, 0)
        
        theta = 0
        ct = cos (theta)
        st = sin (theta)
        body.setRotation([
            ct, 0.0, -st,
            0.0, 1.0, 0.0,
            st, 0.0, ct
        ])
        self.bodies[name] = body

        # Collision callback
        def near_callback(self, args, geom1, geom2) :
            contacts = ode.collide(geom1, geom2)
            # Create contact joints
            world,contactgroup = args
            for c in contacts:
                c.setBounce(0.2)
                c.setMu(5000)
                j = ode.ContactJoint(world, contactgroup, c)
                j.attach(geom1.getBody(), geom2.getBody())

if __name__ == "main" :
    fps = 25.0
    dt = 1.0 / fps
    
    physics = Physics()
    import sys, select
    
    while True :
        # A joint group for the contact joints that are generated whenever
        # two bodies collide
        contactgroup = ode.JointGroup()
        
        # Detect collisions and create contact joints
        space.collide((world,contactgroup), physics.near_callback)
        
        # Simulation step
        world.step(dt)
        
        # Check if there are any commands buffered
        cmd = select.select([ sys.stdin ], [], [], dt / 2)
        
        # Remove all contact joints
        contactgroup.empty()
